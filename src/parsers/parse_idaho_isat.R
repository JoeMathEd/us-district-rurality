#!/usr/bin/env Rscript
# src/parsers/parse_idaho_isat.R
# Parses an Idaho Report Card normalized CSV export containing Grade 3, 5, and 8
# rows (in `Student Group`) into the standardized Tier 2 schema expected by
# 04_fetch_and_clean_assessments.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
raw_file  <- if (length(args) >= 1 && args[1] != "") args[1] else "data_raw/idaho/IdahoReportCard_DataExport_Normalized_20261122_174815.csv"
cache_dir <- if (length(args) >= 2 && args[2] != "") args[2] else "data_cache"
out_csv   <- if (length(args) >= 3 && args[3] != "") args[3] else "data_raw/state_assessments/idaho_isat_math.csv"

if (!file.exists(raw_file)) stop("Idaho Report Card CSV not found: ", raw_file)

# 1. Crosswalk 3-digit Idaho District ID -> 7-digit NCES GEOID from cached CCD
ccd_files <- sort(list.files(cache_dir, pattern = "^nces_ccd_raw_\\d{4}\\.rds$", full.names = TRUE), decreasing = TRUE)
if (length(ccd_files) == 0) stop("No nces_ccd_raw_<year>.rds found in ", cache_dir)

id_xwalk <- readRDS(ccd_files[1]) |>
  filter(fips == 16) |>
  transmute(
    GEOID = str_pad(as.character(leaid), 7, pad = "0"),
    dist_id = str_pad(str_extract(as.character(state_leaid), "\\d+"), 3, pad = "0")
  ) |>
  filter(!is.na(dist_id)) |>
  distinct(dist_id, .keep_all = TRUE)

# 2. Read & filter to District-level, Grades 3, 5, and 8, 4 ISAT Math Levels
raw_df <- read_csv(raw_file, col_types = cols(.default = col_character()), show_col_types = FALSE)

level_map <- c(
  "Math Level Below Basic" = "lvl1",
  "Math Level Basic"       = "lvl2",
  "Math Level Proficient"  = "lvl3",
  "Math Level Advanced"    = "lvl4"
)

grade_map <- c(
  "Grade 3" = "3",
  "Grade 5" = "5",
  "Grade 8" = "8"
)

dist_levels <- raw_df |>
  mutate(
    org_type      = str_trim(`Organization Type`),
    student_group = str_trim(`Student Group`),
    measure       = str_trim(`Measure Label`),
    dist_id       = str_pad(str_trim(`District ID`), 3, pad = "0"),
    school_year   = str_trim(`School Year`),
    rate_num      = suppressWarnings(as.numeric(str_trim(Rate))),
    text_val      = na_if(str_trim(`Text Value`), ""),
    nsize_num     = suppressWarnings(as.integer(str_remove_all(str_trim(NSize), ","))),
    sup_type      = na_if(str_trim(`Suppression Type`), "")
  ) |>
  filter(
    org_type == "District",
    student_group %in% names(grade_map),
    measure %in% names(level_map)
  ) |>
  mutate(
    grade   = unname(grade_map[student_group]),
    lvl_key = unname(level_map[measure])
  )

report_year <- dist_levels$school_year[1] |>
  str_extract("\\d{4}$") |>
  as.integer()

# 3. Reshape to 1 row per district-grade & resolve exact, blurred, and suppressed cells
wide_df <- dist_levels |>
  select(dist_id, `District Name`, grade, lvl_key, rate_num, text_val, nsize_num, sup_type) |>
  pivot_wider(
    names_from = lvl_key,
    values_from = c(rate_num, text_val, nsize_num, sup_type),
    names_sep = "_"
  ) |>
  inner_join(id_xwalk, by = "dist_id")

resolve_district_grade <- function(row) {
  rates <- c(row$rate_num_lvl1, row$rate_num_lvl2, row$rate_num_lvl3, row$rate_num_lvl4)
  texts <- c(row$text_val_lvl1, row$text_val_lvl2, row$text_val_lvl3, row$text_val_lvl4)
  sups  <- c(row$sup_type_lvl1, row$sup_type_lvl2, row$sup_type_lvl3, row$sup_type_lvl4)
  nsize <- coalesce(row$nsize_num_lvl1, row$nsize_num_lvl2, row$nsize_num_lvl3, row$nsize_num_lvl4)
  lbls  <- c("Below Basic", "Basic", "Proficient", "Advanced")
  
  n_exact <- sum(!is.na(rates))
  
  # Case A: All 4 levels reported cleanly
  if (n_exact == 4) {
    return(tibble(
      GEOID = row$GEOID, year = report_year, test_name = "Idaho ISAT Math", grade = row$grade,
      n = nsize,
      lvl1_pct = round(rates[1], 1), lvl2_pct = round(rates[2], 1),
      lvl3_pct = round(rates[3], 1), lvl4_pct = round(rates[4], 1),
      lvl1_label = lbls[1], lvl2_label = lbls[2], lvl3_label = lbls[3], lvl4_label = lbls[4],
      status = "exact", note = NA_character_
    ))
  }
  
  # Case B: Partial BlurredDynamic (1 to 3 exact levels + upper bounds like "<11", "<23")
  if (n_exact >= 1 && n_exact < 4) {
    missing_idx <- which(is.na(rates))
    caps <- suppressWarnings(as.numeric(str_remove_all(texts[missing_idx], "[<>%\\s]")))
    residual <- max(0, 100 - sum(rates, na.rm = TRUE))
    
    if (all(!is.na(caps)) && sum(caps) > 0) {
      est <- rates
      est[missing_idx] <- round(residual * (caps / sum(caps)), 1)
      blurred_desc <- paste0(lbls[missing_idx], " ", texts[missing_idx], "%", collapse = ", ")
      return(tibble(
        GEOID = row$GEOID, year = report_year, test_name = "Idaho ISAT Math", grade = row$grade,
        n = nsize,
        lvl1_pct = round(est[1], 1), lvl2_pct = round(est[2], 1),
        lvl3_pct = round(est[3], 1), lvl4_pct = round(est[4], 1),
        lvl1_label = lbls[1], lvl2_label = lbls[2], lvl3_label = lbls[3], lvl4_label = lbls[4],
        status = "estimated_from_bounds",
        note = paste0("Estimated from Idaho SDE bounds (", blurred_desc, ")")
      ))
    }
  }
  
  # Case C: Fully suppressed (NSize or all 4 levels BlurredDynamic)
  note_txt <- if (any(!is.na(texts))) {
    blurred_all <- paste0(lbls[!is.na(texts)], " ", texts[!is.na(texts)], "%", collapse = ", ")
    paste0("Idaho SDE dynamic blurring (", blurred_all, ")")
  } else if (any(sups == "NSize", na.rm = TRUE)) {
    "Suppressed (small cohort NSize)"
  } else {
    "Suppressed or not applicable"
  }
  
  tibble(
    GEOID = row$GEOID, year = report_year, test_name = "Idaho ISAT Math", grade = row$grade,
    n = nsize,
    lvl1_pct = NA_real_, lvl2_pct = NA_real_, lvl3_pct = NA_real_, lvl4_pct = NA_real_,
    lvl1_label = lbls[1], lvl2_label = lbls[2], lvl3_label = lbls[3], lvl4_label = lbls[4],
    status = "suppressed", note = note_txt
  )
}

out_df <- wide_df |>
  mutate(row_key = paste(GEOID, grade, sep = "_")) |>
  group_split(row_key) |>
  map_dfr(resolve_district_grade) |>
  arrange(GEOID, grade)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write_csv(out_df, out_csv)
message(sprintf("Wrote %d Idaho district-grade rows across %d districts (%d exact, %d estimated_from_bounds, %d suppressed) to %s",
                nrow(out_df),
                n_distinct(out_df$GEOID),
                sum(out_df$status == "exact"),
                sum(out_df$status == "estimated_from_bounds"),
                sum(out_df$status == "suppressed"),
                out_csv))