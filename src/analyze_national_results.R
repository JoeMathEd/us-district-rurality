# src/analyze_national_results.R
# National-level descriptive & comparative summaries for:
# "Measuring the rurality of US School Districts"

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(scales)
  library(sf)
})

# -------------------------------------------------------------------------
# 1. Locate and load processed national dataset
# -------------------------------------------------------------------------
user_scratch <- Sys.getenv("USER", unset = "")
candidate_rds <- c(
  file.path("outputs", "US_School_Districts_Rural_Summary_2024.rds"),
  if (nzchar(user_scratch)) {
    file.path("/bsuscratch", user_scratch, "district-rurality", "outputs", "US_School_Districts_Rural_Summary_2024.rds")
  } else {
    NA_character_
  }
)
candidate_rds <- candidate_rds[!is.na(candidate_rds)]
existing_rds <- candidate_rds[file.exists(candidate_rds)]

if (length(existing_rds) > 0) {
  message("Loading RDS checkpoint: ", existing_rds[1])
  raw_obj <- readRDS(existing_rds[1])
  df <- if (inherits(raw_obj, "sf")) sf::st_drop_geometry(raw_obj) else as_tibble(raw_obj)
} else if (file.exists("docs/data/districts_poc.geojson")) {
  message("RDS not found locally; falling back to docs/data/districts_poc.geojson")
  df <- sf::st_read("docs/data/districts_poc.geojson", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    as_tibble()
} else {
  stop("Could not find US_School_Districts_Rural_Summary_2024.rds or docs/data/districts_poc.geojson")
}

has_col <- function(data, col) col %in% names(data)
num_or_na <- function(data, col) if (has_col(data, col)) suppressWarnings(as.numeric(data[[col]])) else rep(NA_real_, nrow(data))
chr_or_na <- function(data, col) if (has_col(data, col)) as.character(data[[col]]) else rep(NA_character_, nrow(data))
lgl_or_na <- function(data, col) if (has_col(data, col)) as.logical(data[[col]]) else rep(NA, nrow(data))

# Standardize core analytical columns defensively
d <- df |>
  mutate(
    geoid = sprintf("%07s", gsub("\\D", "", chr_or_na(df, "GEOID"))),
    state_abbr = chr_or_na(df, "state_abbr"),
    state_name = chr_or_na(df, "state_name"),
    lea_name = coalesce(chr_or_na(df, "lea_name"), chr_or_na(df, "district_name")),
    locale_code = chr_or_na(df, "urban_centric_locale"),
    locale_type = coalesce(chr_or_na(df, "urban_centric_locale_type"), "Unknown"),
    locale_broad = case_when(
      grepl("^1", locale_code) | grepl("City", locale_type, ignore.case = TRUE) ~ "1: City",
      grepl("^2", locale_code) | grepl("Suburb", locale_type, ignore.case = TRUE) ~ "2: Suburb",
      grepl("^3", locale_code) | grepl("Town", locale_type, ignore.case = TRUE) ~ "3: Town",
      grepl("^4", locale_code) | grepl("Rural", locale_type, ignore.case = TRUE) ~ "4: Rural",
      TRUE ~ "Other/Missing"
    ),
    est_youth_5_17 = coalesce(
      num_or_na(df, "est_total_pop_5_17"),
      num_or_na(df, "est_pop_5_17"),
      num_or_na(df, "est_total_pop")
    ),
    est_rural_youth_5_17 = coalesce(
      num_or_na(df, "est_rural_pop_5_17"),
      if (has_col(df, "pct_pop_rural")) est_youth_5_17 * (num_or_na(df, "pct_pop_rural") / 100) else NA_real_
    ),
    pct_rural_youth = coalesce(
      num_or_na(df, "pct_pop_rural"),
      if_else(est_youth_5_17 > 0, 100 * est_rural_youth_5_17 / est_youth_5_17, NA_real_)
    ),
    pct_outside_ua = num_or_na(df, "pct_youth_outside_ua"),
    pct_ruca_rural = num_or_na(df, "pct_youth_ruca_rural"),
    pct_ruca_nonmetro = num_or_na(df, "pct_youth_ruca_nonmetro"),
    pct_ruca_town_micro = pmax(0, pct_ruca_nonmetro - pct_ruca_rural),
    dist_any_ua_mi = num_or_na(df, "dist_any_ua_mi"),
    dist_50k_ua_mi = num_or_na(df, "dist_50k_ua_mi"),
    ccd_enroll = num_or_na(df, "enrollment"),
    locale_mismatch = lgl_or_na(df, "locale_mismatch"),
    rev_total_pp = num_or_na(df, "rev_total_pp"),
    pct_rev_local = num_or_na(df, "pct_rev_local"),
    exp_cur_pp = num_or_na(df, "exp_cur_pp"),
    exp_cur_cwift_pp = num_or_na(df, "exp_cur_cwift_pp"),
    cwift_est = num_or_na(df, "cwift_est"),
    saipe_stpov_pct = num_or_na(df, "saipe_stpov_pct"),
    est_med_hh_inc = num_or_na(df, "est_med_hh_inc"),
    acs_poverty_pct = num_or_na(df, "acs_poverty_pct"),
    work_pct_ag_for = num_or_na(df, "work_pct_ag_for"),
    work_pct_mfg = num_or_na(df, "work_pct_mfg"),
    res_pct_ag_mining = num_or_na(df, "res_pct_ag_mining"),
    commute_self_contain_pct = num_or_na(df, "commute_self_contain_pct")
  )

cat("\n====================================================================\n")
cat("Q1. NATIONAL SCALE: DISTRICTS & SCHOOL-AGED RURAL YOUTH (AGES 5-17)\n")
cat("====================================================================\n")
n_dist <- nrow(d)
n_states <- n_distinct(na.omit(d$state_abbr))
tot_youth <- sum(d$est_youth_5_17, na.rm = TRUE)
tot_rural_youth <- sum(d$est_rural_youth_5_17, na.rm = TRUE)
nat_rural_share <- if (tot_youth > 0) 100 * tot_rural_youth / tot_youth else NA_real_

n_maj_50 <- sum(d$pct_rural_youth >= 50, na.rm = TRUE)
n_high_75 <- sum(d$pct_rural_youth >= 75, na.rm = TRUE)
n_full_100 <- sum(d$pct_rural_youth >= 99.99, na.rm = TRUE)
n_zero_0 <- sum(d$pct_rural_youth <= 0.01, na.rm = TRUE)

cat(sprintf("States / Jurisdictions covered: %d\n", n_states))
cat(sprintf("Total districts analyzed: %s\n", comma(n_dist)))
cat(sprintf("Total est. youth (ages 5-17): %s\n", comma(round(tot_youth))))
cat(sprintf("Total est. rural youth (<500/mi2): %s (%.1f%% of all youth)\n", comma(round(tot_rural_youth)), nat_rural_share))
cat(sprintf("Majority-rural districts (>=50%%): %s (%.1f%%)\n", comma(n_maj_50), 100 * n_maj_50 / n_dist))
cat(sprintf("Highly rural districts (>=75%%): %s (%.1f%%)\n", comma(n_high_75), 100 * n_high_75 / n_dist))
cat(sprintf("100%% rural districts: %s (%.1f%%) | 0%% rural districts: %s (%.1f%%)\n",
            comma(n_full_100), 100 * n_full_100 / n_dist,
            comma(n_zero_0), 100 * n_zero_0 / n_dist))

cat("\n====================================================================\n")
cat("Q2. MULTI-DIMENSIONAL RURALITY & REMOTENESS ACROSS DISTRICTS\n")
cat("====================================================================\n")
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

multidim_tbl <- tibble(
  indicator = c(
    "Block-dasymetric rural (<500/mi2)",
    "Outside Census Urban Area",
    "USDA RUCA Rural (Codes 7-10)",
    "USDA RUCA Nonmetro (Codes 4-10)",
    "USDA RUCA Town/Micro (Codes 4-6)"
  ),
  unweighted_district_mean_pct = c(
    mean(d$pct_rural_youth, na.rm = TRUE),
    mean(d$pct_outside_ua, na.rm = TRUE),
    mean(d$pct_ruca_rural, na.rm = TRUE),
    mean(d$pct_ruca_nonmetro, na.rm = TRUE),
    mean(d$pct_ruca_town_micro, na.rm = TRUE)
  ),
  youth_weighted_pct = c(
    w_mean(d$pct_rural_youth, d$est_youth_5_17),
    w_mean(d$pct_outside_ua, d$est_youth_5_17),
    w_mean(d$pct_ruca_rural, d$est_youth_5_17),
    w_mean(d$pct_ruca_nonmetro, d$est_youth_5_17),
    w_mean(d$pct_ruca_town_micro, d$est_youth_5_17)
  )
)
print(multidim_tbl, n = Inf, digits = 1)

cat("\nDistance to Urban Areas (miles, district-level):\n")
print(
  tibble(
    metric = c("dist_any_ua_mi", "dist_50k_ua_mi"),
    median_mi = c(median(d$dist_any_ua_mi, na.rm = TRUE), median(d$dist_50k_ua_mi, na.rm = TRUE)),
    iqr_mi = c(IQR(d$dist_any_ua_mi, na.rm = TRUE), IQR(d$dist_50k_ua_mi, na.rm = TRUE)),
    mean_mi = c(mean(d$dist_any_ua_mi, na.rm = TRUE), mean(d$dist_50k_ua_mi, na.rm = TRUE))
  ),
  digits = 1
)

cat("\n====================================================================\n")
cat("Q3. BREAKDOWN BY NCES URBAN-CENTRIC LOCALE & LOCALE MISMATCHES\n")
cat("====================================================================\n")
locale_tbl <- d |>
  group_by(locale_broad) |>
  summarise(
    n_districts = n(),
    est_youth_5_17 = sum(est_youth_5_17, na.rm = TRUE),
    est_rural_youth = sum(est_rural_youth_5_17, na.rm = TRUE),
    mean_pct_rural = mean(pct_rural_youth, na.rm = TRUE),
    median_pct_rural = median(pct_rural_youth, na.rm = TRUE),
    maj_rural_share_pct = 100 * mean(pct_rural_youth >= 50, na.rm = TRUE),
    n_mismatch = sum(locale_mismatch %in% TRUE),
    .groups = "drop"
  ) |>
  arrange(locale_broad)
print(locale_tbl, n = Inf, digits = 1)

if (any(!is.na(d$locale_mismatch))) {
  mm_rural_youth <- sum(d$est_rural_youth_5_17[d$locale_mismatch %in% TRUE & d$locale_broad != "4: Rural"], na.rm = TRUE)
  cat(sprintf("\nTotal flagged locale mismatches: %s (%.1f%% of districts)\n",
              comma(sum(d$locale_mismatch %in% TRUE)),
              100 * mean(d$locale_mismatch %in% TRUE)))
  cat(sprintf("Rural youth in mismatched non-Rural districts: %s\n", comma(round(mm_rural_youth))))
}

cat("\n====================================================================\n")
cat("Q4. STATE-LEVEL SUMMARY (TOP 10 STATES BY RURAL YOUTH COUNT)\n")
cat("====================================================================\n")
state_tbl <- d |>
  group_by(state_abbr, state_name) |>
  summarise(
    n_districts = n(),
    est_youth_5_17 = sum(est_youth_5_17, na.rm = TRUE),
    est_rural_youth = sum(est_rural_youth_5_17, na.rm = TRUE),
    state_rural_youth_pct = if_else(sum(est_youth_5_17, na.rm = TRUE) > 0,
                                    100 * sum(est_rural_youth_5_17, na.rm = TRUE) / sum(est_youth_5_17, na.rm = TRUE),
                                    NA_real_),
    pct_districts_maj_rural = 100 * mean(pct_rural_youth >= 50, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(est_rural_youth))

print(head(state_tbl, 10), n = 10, digits = 1)

cat("\nTop 10 states by share of districts that are majority-rural (>=50%):\n")
print(state_tbl |> arrange(desc(pct_districts_maj_rural), desc(n_districts)) |> head(10), n = 10, digits = 1)

cat("\n====================================================================\n")
cat("Q5. FISCAL CAPACITY, CWIFT & ECONOMY ACROSS RURALITY TIERS\n")
cat("====================================================================\n")
econ_tbl <- d |>
  mutate(
    rural_tier = case_when(
      is.na(pct_rural_youth) ~ "Missing",
      pct_rural_youth < 25 ~ "0–24.9% Rural",
      pct_rural_youth < 50 ~ "25–49.9% Rural",
      pct_rural_youth < 75 ~ "50–74.9% Rural",
      TRUE ~ "75–100% Rural"
    )
  ) |>
  group_by(rural_tier) |>
  summarise(
    n_districts = n(),
    med_rev_pp = median(rev_total_pp, na.rm = TRUE),
    med_local_rev_pct = median(pct_rev_local, na.rm = TRUE),
    med_exp_cur_pp = median(exp_cur_pp, na.rm = TRUE),
    med_exp_cwift_pp = median(exp_cur_cwift_pp, na.rm = TRUE),
    med_cwift = median(cwift_est, na.rm = TRUE),
    med_hh_inc = median(est_med_hh_inc, na.rm = TRUE),
    med_work_ag_for_pct = median(work_pct_ag_for, na.rm = TRUE),
    med_commute_stay_pct = median(commute_self_contain_pct, na.rm = TRUE),
    .groups = "drop"
  )
print(econ_tbl, n = Inf, digits = 1)

