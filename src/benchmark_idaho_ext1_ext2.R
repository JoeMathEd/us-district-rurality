# src/benchmark_idaho_ext1_ext2.R
# Prototype & benchmark Extension #1 (School District Finance via edfinr)
# and Extension #2 (ACS Socioeconomic/Industry Mix + LODES8 Workplace & Commute Flows)
# across Idaho's 115 bounded school districts before national deployment.

library(tidyverse)
library(sf)
library(tidycensus)
library(edfinr)
library(lehdr)
library(units)

sf_use_s2(FALSE)
options(lehdr_use_cache = TRUE)

cache_dir <- "/bsuscratch/joechampion/district-rurality/data_cache"
year_this <- as.integer(trimws(readLines(file.path(cache_dir, "latest_cached_year.txt"), n = 1)))

# 1. Load Cached Idaho Spatial Checkpoint & 2020 Block Points ----
message("Loading cached Idaho spatial checkpoint & 2020 block points...")
districts_sf <- readRDS(file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))) |>
  filter(state_fip == "16", !st_is_empty(geometry)) |>
  st_transform(5070)

id_blocks <- readRDS(file.path(cache_dir, "decennial_block_points_2020.rds")) |>
  filter(state_fip == "16") |>
  st_transform(5070)

blk_to_district <- st_join(
  id_blocks |> select(block_geoid, tract_geoid, blk_pop, blk_hu),
  districts_sf |> select(GEOID),
  join = st_intersects,
  left = FALSE
) |>
  st_drop_geometry()

tract_denoms <- id_blocks |>
  st_drop_geometry() |>
  group_by(tract_geoid) |>
  summarize(
    tract_blk_pop = sum(blk_pop, na.rm = TRUE),
    tract_blk_hu  = sum(blk_hu, na.rm = TRUE),
    tract_n_blks  = n(),
    .groups = "drop"
  )

dasymetric_weights <- blk_to_district |>
  left_join(tract_denoms, by = "tract_geoid") |>
  mutate(
    blk_weight = case_when(
      tract_blk_pop > 0 ~ blk_pop / tract_blk_pop,
      tract_blk_hu > 0  ~ blk_hu / tract_blk_hu,
      tract_n_blks > 0  ~ 1 / tract_n_blks,
      TRUE              ~ 0
    )
  ) |>
  group_by(GEOID, tract_geoid) |>
  summarize(dasymetric_prop = pmin(sum(blk_weight, na.rm = TRUE), 1.0), .groups = "drop")

# ==============================================================================
# EXTENSION #1: SCHOOL DISTRICT FINANCE (edfinr)
# ==============================================================================
message("Fetching/loading Idaho F-33 School District Finance data via edfinr...")
fin_cache <- file.path(cache_dir, "idaho_edfinr_benchmark.rds")

if (file.exists(fin_cache)) {
  id_fin_raw <- readRDS(fin_cache)
} else {
  id_fin_raw <- NULL
  for (fy in seq(year_this, 2020L, by = -1L)) {
    res <- tryCatch(
      edfinr::get_finance_data(yr = fy, geo = "ID", cpi_adj = "none"),
      error = \(e) NULL
    )
    if (!is.null(res) && nrow(res) > 0) {
      message("  Found edfinr finance data for fiscal year: ", fy)
      id_fin_raw <- res
      break
    }
  }
  if (is.null(id_fin_raw)) stop("Could not retrieve edfinr finance data for Idaho.")
  saveRDS(id_fin_raw, fin_cache)
}

id_fin_clean <- id_fin_raw |>
  mutate(GEOID = stringr::str_pad(as.character(ncesid), 7, pad = "0")) |>
  transmute(
    GEOID,
    fin_year            = year,
    fin_enroll          = enroll,
    rev_total_pp        = rev_total_pp,
    rev_local_pp        = rev_local_pp,
    rev_state_pp        = rev_state_pp,
    rev_fed_pp          = rev_fed_pp,
    pct_rev_local       = if_else(!is.na(rev_total) & rev_total > 0, 100 * rev_local / rev_total, NA_real_),
    pct_rev_state       = if_else(!is.na(rev_total) & rev_total > 0, 100 * rev_state / rev_total, NA_real_),
    pct_rev_fed         = if_else(!is.na(rev_total) & rev_total > 0, 100 * rev_fed / rev_total, NA_real_),
    exp_cur_pp          = exp_cur_pp,
    exp_cap_total_pp    = exp_cap_total_pp,
    cwift_est           = cwift_est,
    exp_cur_cwift_pp    = if_else(!is.na(cwift_est) & cwift_est > 0, exp_cur_pp / cwift_est, NA_real_),
    saipe_stpov_pct     = if_else(!is.na(stpov_pct), stpov_pct * 100, NA_real_),
    edfinr_mhi          = mhi,
    edfinr_ba_plus_pct  = if_else(!is.na(ba_plus_pct), ba_plus_pct * 100, NA_real_),
    edfinr_unemp_pct    = if_else(!is.na(unemp_rate), unemp_rate * 100, NA_real_)
  ) |>
  distinct(GEOID, .keep_all = TRUE)

# ==============================================================================
# EXTENSION #2A: CENSUS ACS SOCIOECONOMIC & RESIDENT INDUSTRY MIX (Tract -> District)
# ==============================================================================
message("Fetching/loading Idaho ACS Socioeconomic & C24030 Industry Mix...")
acs_econ_cache <- file.path(cache_dir, paste0("idaho_acs_econ_", year_this, ".rds"))

if (file.exists(acs_econ_cache)) {
  id_acs_econ_raw <- readRDS(acs_econ_cache)
} else {
  econ_vars <- c(
    med_hh_inc   = "B19013_001",
    hh_total     = "B19001_001",
    pov_univ     = "B17001_001",
    pov_below    = "B17001_002",
    lf_civ       = "B23025_003",
    lf_unemp     = "B23025_005",
    emp_total    = "C24030_001",
    ag_min_m     = "C24030_003", ag_min_f     = "C24030_030",
    const_m      = "C24030_006", const_f      = "C24030_033",
    mfg_m        = "C24030_007", mfg_f        = "C24030_034",
    retail_m     = "C24030_009", retail_f     = "C24030_036",
    ed_hlth_m    = "C24030_014", ed_hlth_f    = "C24030_041"
  )
  id_acs_econ_raw <- get_acs(
    geography = "tract",
    variables = econ_vars,
    state     = "16",
    year      = year_this,
    geometry  = FALSE,
    output    = "wide"
  )
  saveRDS(id_acs_econ_raw, acs_econ_cache)
}

id_acs_econ_dist <- dasymetric_weights |>
  inner_join(
    id_acs_econ_raw |>
      transmute(
        tract_geoid   = GEOID,
        med_hh_inc    = med_hh_incE,
        hh_total      = coalesce(hh_totalE, 0),
        pov_univ      = coalesce(pov_univE, 0),
        pov_below     = coalesce(pov_belowE, 0),
        lf_civ        = coalesce(lf_civE, 0),
        lf_unemp      = coalesce(lf_unempE, 0),
        emp_total     = coalesce(emp_totalE, 0),
        emp_ag_mining = coalesce(ag_min_mE, 0) + coalesce(ag_min_fE, 0),
        emp_const     = coalesce(const_mE, 0) + coalesce(const_fE, 0),
        emp_mfg       = coalesce(mfg_mE, 0) + coalesce(mfg_fE, 0),
        emp_retail    = coalesce(retail_mE, 0) + coalesce(retail_fE, 0),
        emp_ed_hlth   = coalesce(ed_hlth_mE, 0) + coalesce(ed_hlth_fE, 0)
      ),
    by = "tract_geoid"
  ) |>
  group_by(GEOID) |>
  summarize(
    est_med_hh_inc    = weighted.mean(med_hh_inc, hh_total * dasymetric_prop, na.rm = TRUE),
    acs_poverty_pct   = if_else(sum(pov_univ * dasymetric_prop) > 0,
                                100 * sum(pov_below * dasymetric_prop) / sum(pov_univ * dasymetric_prop), NA_real_),
    acs_unemp_pct     = if_else(sum(lf_civ * dasymetric_prop) > 0,
                                100 * sum(lf_unemp * dasymetric_prop) / sum(lf_civ * dasymetric_prop), NA_real_),
    res_pct_ag_mining = if_else(sum(emp_total * dasymetric_prop) > 0,
                                100 * sum(emp_ag_mining * dasymetric_prop) / sum(emp_total * dasymetric_prop), NA_real_),
    res_pct_mfg       = if_else(sum(emp_total * dasymetric_prop) > 0,
                                100 * sum(emp_mfg * dasymetric_prop) / sum(emp_total * dasymetric_prop), NA_real_),
    res_pct_ed_hlth   = if_else(sum(emp_total * dasymetric_prop) > 0,
                                100 * sum(emp_ed_hlth * dasymetric_prop) / sum(emp_total * dasymetric_prop), NA_real_),
    .groups = "drop"
  )

# ==============================================================================
# EXTENSION #2B: LEHD LODES8 WORKPLACE INDUSTRY MIX & COMMUTE SELF-CONTAINMENT
# ==============================================================================
message("Fetching/loading Idaho LEHD LODES8 WAC & OD block-level data...")
lodes_cache <- file.path(cache_dir, "idaho_lodes8_benchmark.rds")

if (file.exists(lodes_cache)) {
  lodes_list <- readRDS(lodes_cache)
} else {
  lodes_yr <- NA_integer_
  wac_raw  <- NULL
  for (yr in 2024:2021) {
    res <- tryCatch(
      lehdr::grab_lodes(state = "id", year = yr, version = "LODES8", lodes_type = "wac",
                        job_type = "JT01", segment = "S000", agg_geo = "block",
                        download_dir = cache_dir, use_cache = TRUE),
      error = \(e) NULL
    )
    if (!is.null(res) && nrow(res) > 0) {
      lodes_yr <- yr
      wac_raw  <- res
      message("  Found LODES8 data for year: ", lodes_yr)
      break
    }
  }
  od_main <- lehdr::grab_lodes(state = "id", year = lodes_yr, version = "LODES8", lodes_type = "od",
                               job_type = "JT01", segment = "S000", state_part = "main",
                               agg_geo = "block", download_dir = cache_dir, use_cache = TRUE)
  lodes_list <- list(year = lodes_yr, wac = wac_raw, od_main = od_main)
  saveRDS(lodes_list, lodes_cache)
}

blk_key <- blk_to_district |> select(block_geoid, GEOID)

lodes_wac_dist <- lodes_list$wac |>
  mutate(block_geoid = stringr::str_pad(as.character(w_geocode), 15, pad = "0")) |>
  inner_join(blk_key, by = "block_geoid") |>
  group_by(GEOID) |>
  summarize(
    lodes_year        = lodes_list$year,
    work_total_jobs   = sum(C000, na.rm = TRUE),
    work_pct_ag_for   = if_else(work_total_jobs > 0, 100 * sum(CNS01, na.rm = TRUE) / work_total_jobs, NA_real_),
    work_pct_mining   = if_else(work_total_jobs > 0, 100 * sum(CNS02, na.rm = TRUE) / work_total_jobs, NA_real_),
    work_pct_mfg      = if_else(work_total_jobs > 0, 100 * sum(CNS05, na.rm = TRUE) / work_total_jobs, NA_real_),
    work_pct_ed_hlth  = if_else(work_total_jobs > 0, 100 * sum(CNS15 + CNS16, na.rm = TRUE) / work_total_jobs, NA_real_),
    work_pct_low_earn = if_else(work_total_jobs > 0, 100 * sum(CE01, na.rm = TRUE) / work_total_jobs, NA_real_),
    work_pct_hi_earn  = if_else(work_total_jobs > 0, 100 * sum(CE03, na.rm = TRUE) / work_total_jobs, NA_real_),
    .groups = "drop"
  )

od_mapped <- lodes_list$od_main |>
  mutate(
    h_blk = stringr::str_pad(as.character(h_geocode), 15, pad = "0"),
    w_blk = stringr::str_pad(as.character(w_geocode), 15, pad = "0")
  ) |>
  left_join(blk_key |> rename(h_geoid = GEOID), by = c("h_blk" = "block_geoid")) |>
  left_join(blk_key |> rename(w_geoid = GEOID), by = c("w_blk" = "block_geoid"))

res_workers <- od_mapped |>
  filter(!is.na(h_geoid)) |>
  group_by(GEOID = h_geoid) |>
  summarize(
    resident_workers_instate = sum(S000, na.rm = TRUE),
    internal_commuters       = sum(S000[h_geoid == w_geoid], na.rm = TRUE),
    commute_self_contain_pct = if_else(resident_workers_instate > 0,
                                       100 * internal_commuters / resident_workers_instate, NA_real_),
    .groups = "drop"
  )

# ==============================================================================
# MERGE & BENCHMARK SUMMARY TABLES
# ==============================================================================
id_bench_12 <- districts_sf |>
  st_drop_geometry() |>
  left_join(id_fin_clean, by = "GEOID") |>
  left_join(id_acs_econ_dist, by = "GEOID") |>
  left_join(lodes_wac_dist, by = "GEOID") |>
  left_join(res_workers, by = "GEOID")

cat("\n=== IDAHO EXT #1 (FINANCE) & EXT #2 (ECONOMY) BY NCES LOCALE ===\n")
id_bench_12 |>
  group_by(urban_centric_locale_type) |>
  summarize(
    n_dist               = n(),
    med_rev_total_pp     = round(median(rev_total_pp, na.rm = TRUE), 0),
    med_pct_rev_local    = round(median(pct_rev_local, na.rm = TRUE), 1),
    med_exp_cur_pp       = round(median(exp_cur_pp, na.rm = TRUE), 0),
    med_cwift            = round(median(cwift_est, na.rm = TRUE), 3),
    med_hh_inc           = round(median(est_med_hh_inc, na.rm = TRUE), 0),
    med_work_ag_min_pct  = round(median(work_pct_ag_for + work_pct_mining, na.rm = TRUE), 1),
    med_self_contain_pct = round(median(commute_self_contain_pct, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  print(n = 20)

cat("\n=== CORRELATIONS WITH RURALITY & REMOTENESS (IDAHO, n=115) ===\n")
id_bench_12 |>
  select(pct_pop_rural, pct_youth_outside_ua, dist_50k_ua_mi,
         rev_total_pp, pct_rev_local, exp_cur_pp, cwift_est,
         est_med_hh_inc, acs_poverty_pct, work_pct_ag_for, commute_self_contain_pct) |>
  cor(use = "pairwise.complete.obs") |>
  round(3) |>
  print()

saveRDS(id_bench_12, file.path(cache_dir, "idaho_ext1_ext2_benchmark.rds"))