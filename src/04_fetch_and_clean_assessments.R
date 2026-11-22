#!/usr/bin/env Rscript
# 04_fetch_and_clean_assessments.R
# Fetches Grade 3, 5, and 8 district math assessment data (Tier 1: National EDFacts;
# Tier 2: State DOE 4-level report card overrides) and exports docs/data/math_assessments.json.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(jsonlite)
  library(educationdata)
})

args <- commandArgs(trailingOnly = TRUE)
cache_dir <- if (length(args) >= 1 && args[1] != "") args[1] else "data_cache"
web_data_dir <- if (length(args) >= 2 && args[2] != "") args[2] else "docs/data"
state_override_dir <- if (length(args) >= 3 && args[3] != "") args[3] else "data_raw/state_assessments"

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(web_data_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load cached CCD directory to validate 7-digit GEOIDs & map state_leaid -> GEOID
ccd_files <- sort(list.files(cache_dir, pattern = "^nces_ccd_raw_\\d{4}\\.rds$", full.names = TRUE), decreasing = TRUE)
if (length(ccd_files) == 0) stop("No nces_ccd_raw_<year>.rds found in ", cache_dir)

ccd_df <- readRDS(ccd_files[1]) |>
  transmute(
    GEOID = str_pad(as.character(leaid), 7, pad = "0"),
    state_fips = str_pad(as.character(fips), 2, pad = "0"),
    state_leaid = as.character(state_leaid)
  ) |>
  distinct(GEOID, .keep_all = TRUE)

# 2. Probe latest available EDFacts district assessment year
probe_edfacts_year <- function(candidate_years = 2024:2018) {
  for (yr in candidate_years) {
    ok <- tryCatch({
      test <- suppressMessages(get_education_data(
        level = "school-districts",
        source = "edfacts",
        topic = "assessments",
        filters = list(year = yr, grade_edfacts = 3, fips = 11)
      ))
      nrow(test) > 0
    }, error = \(e) FALSE)
    if (isTRUE(ok)) return(yr)
  }
  stop("Could not find an available EDFacts district assessments year.")
}

edfacts_year <- probe_edfacts_year()
message("Using EDFacts district math assessments year: ", edfacts_year)

edfacts_cache_file <- file.path(cache_dir, sprintf("edfacts_math_g358_%d.rds", edfacts_year))

if (file.exists(edfacts_cache_file)) {
  message("Loading cached EDFacts math assessments: ", edfacts_cache_file)
  edfacts_raw <- readRDS(edfacts_cache_file)
} else {
  message("Fetching EDFacts Grades 3, 5, 8 math assessments by state...")
  all_fips <- sort(unique(as.integer(ccd_df$state_fips)))
  all_fips <- all_fips[!is.na(all_fips) & all_fips <= 56]
  
  edfacts_raw <- map_dfr(all_fips, function(st_fips) {
    message(sprintf("  Fetching FIPS %02d...", st_fips))
    Sys.sleep(0.25)
    tryCatch({
      suppressMessages(get_education_data(
        level = "school-districts",
        source = "edfacts",
        topic = "assessments",
        filters = list(year = edfacts_year, grade_edfacts = c(3, 5, 8), fips = st_fips)
      ))
    }, error = function(e) {
      warning(sprintf("  Skipped FIPS %02d: %s", st_fips, e$message))
      tibble()
    })
  })
  saveRDS(edfacts_raw, edfacts_cache_file)
  message("Saved EDFacts cache to: ", edfacts_cache_file)
}

# 3. Clean Tier 1 EDFacts records (all-students totals where disaggregators are missing/99)
edfacts_clean <- edfacts_raw |>
  mutate(
    GEOID = str_pad(as.character(leaid), 7, pad = "0"),
    grade = as.character(grade_edfacts)
  ) |>
  filter(grade %in% c("3", "5", "8"), GEOID %in% ccd_df$GEOID)

# Keep aggregate "all students" rows if subgroup columns exist
for (col in intersect(c("race", "sex", "lep", "disability", "econ_disadvantaged", "homeless", "migrant", "foster_care"), names(edfacts_clean))) {
  edfacts_clean <- edfacts_clean |>
    filter(is.na(.data[[col]]) | .data[[col]] %in% c(99, "99", "All"))
}

# Extract math proficiency rate (handles midpoint, low/high interval bounds, and suppression codes < 0)
prof_col <- intersect(c("math_test_pct_prof_midpt", "pct_prof_midpt"), names(edfacts_clean))[1]
low_col  <- intersect(c("math_test_pct_prof_low", "pct_prof_low"), names(edfacts_clean))[1]
high_col <- intersect(c("math_test_pct_prof_high", "pct_prof_high"), names(edfacts_clean))[1]
num_col  <- intersect(c("math_test_num_valid", "num_valid"), names(edfacts_clean))[1]

edfacts_grades <- edfacts_clean |>
  transmute(
    GEOID,
    grade,
    n = if (!is.na(num_col) && !is.null(num_col)) {
      val <- suppressWarnings(as.numeric(.data[[num_col]]))
      if_else(!is.na(val) & val >= 0, as.integer(round(val)), NA_integer_)
    } else NA_integer_,
    mid = suppressWarnings(as.numeric(.data[[prof_col]])),
    low = if (!is.na(low_col) && !is.null(low_col)) suppressWarnings(as.numeric(.data[[low_col]])) else NA_real_,
    high = if (!is.na(high_col) && !is.null(high_col)) suppressWarnings(as.numeric(.data[[high_col]])) else NA_real_
  ) |>
  distinct(GEOID, grade, .keep_all = TRUE) |>
  mutate(
    prof_pct = case_when(
      !is.na(mid) & mid >= 0 & mid <= 100 ~ round(mid, 1),
      !is.na(low) & !is.na(high) & low >= 0 & high <= 100 ~ round((low + high) / 2, 1),
      TRUE ~ NA_real_
    ),
    status = case_when(
      is.na(prof_pct) ~ "suppressed",
      !is.na(low) & !is.na(high) & low != high ~ "midpoint_estimate",
      TRUE ~ "exact"
    ),
    note = case_when(
      status == "midpoint_estimate" ~ sprintf("EDFacts interval: %g%%–%g%% proficient", low, high),
      status == "suppressed" ~ "Suppressed for small cohort privacy",
      TRUE ~ NA_character_
    )
  )

# Build named list keyed by GEOID for Tier 1
tier1_list <- edfacts_grades |>
  group_by(GEOID) |>
  group_split() |>
  set_names(map_chr(., ~ .x$GEOID[1])) |>
  map(function(df_g) {
    grades_out <- list()
    for (g in c("3", "5", "8")) {
      row <- df_g |> filter(grade == g)
      if (nrow(row) == 0 || is.na(row$prof_pct[1])) {
        grades_out[[g]] <- list(
          n = if (nrow(row) > 0) row$n[1] else NULL,
          pct = NULL,
          status = "suppressed",
          note = "Suppressed or unavailable"
        )
      } else {
        p_prof <- row$prof_pct[1]
        p_not  <- round(100 - p_prof, 1)
        grades_out[[g]] <- list(
          n = row$n[1],
          pct = c(p_not, p_prof),
          status = row$status[1],
          note = if (is.na(row$note[1])) NULL else row$note[1]
        )
      }
    }
    list(
      year = edfacts_year,
      test_name = "EDFacts Math",
      levels = c("Not Proficient", "Proficient or Above"),
      palette = c("#e07a5f", "#3d5a80"),
      grades = grades_out
    )
  })

# 4. Tier 2 Hook: Optional State DOE 4-level overrides (CSV files placed in data_raw/state_assessments/)
# Expected standardized CSV columns if you drop a cleaned state file in data_raw/state_assessments/:
# GEOID, year, test_name, grade (3/5/8), n, lvl1_pct, lvl2_pct, lvl3_pct, lvl4_pct,
# lvl1_label, lvl2_label, lvl3_label, lvl4_label, status, note
override_files <- list.files(state_override_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(override_files) > 0) {
  message("Applying Tier 2 state 4-level assessment overrides from: ", state_override_dir)
  overrides_df <- map_dfr(override_files, ~ read.csv(.x, colClasses = c(GEOID = "character", grade = "character"))) |>
    mutate(GEOID = str_pad(GEOID, 7, pad = "0")) |>
    filter(grade %in% c("3", "5", "8"), GEOID %in% ccd_df$GEOID)
  
  tier2_list <- overrides_df |>
    group_by(GEOID) |>
    group_split() |>
    set_names(map_chr(., ~ .x$GEOID[1])) |>
    map(function(df_g) {
      r1 <- df_g[1, ]
      lvl_names <- c(
        if ("lvl1_label" %in% names(r1) && !is.na(r1$lvl1_label)) r1$lvl1_label else "Level 1",
        if ("lvl2_label" %in% names(r1) && !is.na(r1$lvl2_label)) r1$lvl2_label else "Level 2",
        if ("lvl3_label" %in% names(r1) && !is.na(r1$lvl3_label)) r1$lvl3_label else "Level 3",
        if ("lvl4_label" %in% names(r1) && !is.na(r1$lvl4_label)) r1$lvl4_label else "Level 4"
      )
      grades_out <- list()
      for (g in c("3", "5", "8")) {
        row <- df_g |> filter(grade == g)
        if (nrow(row) == 0 || any(is.na(c(row$lvl1_pct[1], row$lvl2_pct[1], row$lvl3_pct[1], row$lvl4_pct[1])))) {
          grades_out[[g]] <- list(
            n = if (nrow(row) > 0 && !is.na(row$n[1])) as.integer(row$n[1]) else NULL,
            pct = NULL,
            status = "suppressed",
            note = if (nrow(row) > 0 && "note" %in% names(row) && !is.na(row$note[1])) row$note[1] else "Suppressed for small cohort privacy"
          )
        } else {
          grades_out[[g]] <- list(
            n = if (!is.na(row$n[1])) as.integer(row$n[1]) else NULL,
            pct = round(c(row$lvl1_pct[1], row$lvl2_pct[1], row$lvl3_pct[1], row$lvl4_pct[1]), 1),
            status = if ("status" %in% names(row) && !is.na(row$status[1])) row$status[1] else "exact",
            note = if ("note" %in% names(row) && !is.na(row$note[1])) row$note[1] else NULL
          )
        }
      }
      list(
        year = as.integer(r1$year),
        test_name = as.character(r1$test_name),
        levels = lvl_names,
        palette = c("#d95f02", "#fc8d62", "#66c2a5", "#1b9e77"),
        grades = grades_out
      )
    })
  
  tier1_list[names(tier2_list)] <- tier2_list
}

out_json <- file.path(web_data_dir, "math_assessments.json")
write_json(tier1_list, out_json, auto_unbox = TRUE, null = "null")
message(sprintf("Exported math assessments for %d districts to %s (%.2f MB)",
                length(tier1_list), out_json, file.info(out_json)$size / 1e6))