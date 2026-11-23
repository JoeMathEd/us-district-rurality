# src/benchmark_idaho_rurality.R
# Benchmark Extension #4 (ACS Ages 5-17 + Decennial Block Dasymetric Weighting)
# against the current Area-Weighted Total Population baseline for Idaho (FIPS 16).

args <- commandArgs(trailingOnly = TRUE)
cache_dir  <- if (length(args) >= 1) args[1] else "data_cache"
output_dir <- if (length(args) >= 2) args[2] else file.path("outputs", "benchmarks")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(units)

options(tigris_use_cache = TRUE)
options(tigris_cache_dir = cache_dir)
sf_use_s2(TRUE)

# 1. Resolve Year & Load From Cache (or Fetch Idaho Directly if Local) ----
library(educationdata)

manifest_path <- file.path(cache_dir, "latest_cached_year.txt")
year_this <- if (file.exists(manifest_path)) {
  as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE)))
} else {
  2023L # Fallback to most recent common ACS/CCD year; adjust if needed
}
message("Benchmarking Idaho (FIPS 16) for year: ", year_this)

tracts_file    <- file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))
districts_file <- file.path(cache_dir, paste0("districts_geo_raw_", year_this, ".rds"))
nces_file      <- file.path(cache_dir, paste0("nces_ccd_raw_", year_this, ".rds"))

if (all(file.exists(c(tracts_file, districts_file, nces_file)))) {
  tracts_prod   <- readRDS(tracts_file) |> filter(str_starts(GEOID, "16"))
  districts_raw <- readRDS(districts_file) |> filter(STATEFP == "16")
  nces_ccd_raw  <- readRDS(nces_file) |> filter(fips == 16)
} else {
  message("National cache not found locally; fetching Idaho-only baseline inputs...")
  id_local_cache <- file.path(cache_dir, paste0("idaho_baseline_", year_this, ".rds"))
  if (file.exists(id_local_cache)) {
    id_inputs     <- readRDS(id_local_cache)
    tracts_prod   <- id_inputs$tracts
    districts_raw <- id_inputs$districts
    nces_ccd_raw  <- id_inputs$nces
  } else {
    tracts_prod <- get_acs(
      geography = "tract", variables = c(total_pop = "B01001_001"),
      state = "16", year = year_this, geometry = TRUE, output = "wide"
    )
    districts_raw <- map(c("unified", "secondary", "elementary"), \(t) {
      tryCatch(
        school_districts(state = "16", type = t, year = year_this, progress_bar = FALSE) |>
          select(GEOID, STATEFP, NAME, geometry) |>
          mutate(tiger_district_type = t),
        error = \(e) NULL
      )
    }) |> compact() |> bind_rows() |> distinct(GEOID, .keep_all = TRUE)
    nces_ccd_raw <- get_education_data(
      level = "school-districts", source = "ccd", topic = "directory",
      filters = list(year = year_this, fips = 16)
    )
    saveRDS(list(tracts = tracts_prod, districts = districts_raw, nces = nces_ccd_raw), id_local_cache)
  }
}

# 2. Fetch & Cache Idaho ACS Table B09001 (Ages 5-17) ----
id_b09001_cache <- file.path(cache_dir, paste0("idaho_acs_b09001_", year_this, ".rds"))
if (file.exists(id_b09001_cache)) {
  id_acs_age <- readRDS(id_b09001_cache)
} else {
  message("Fetching Idaho ACS B09001 (Population Under 18 by Age)...")
  age_vars <- c(
    total_pop = "B01001_001",
    age_5     = "B09001_006",
    age_6_8   = "B09001_007",
    age_9_11  = "B09001_008",
    age_12_14 = "B09001_009",
    age_15_17 = "B09001_010"
  )
  id_acs_age <- get_acs(
    geography = "tract",
    variables = age_vars,
    state = "16",
    year = year_this,
    geometry = FALSE,
    output = "wide"
  ) |>
    mutate(
      pop_5_17 = coalesce(age_5E, 0) + coalesce(age_6_8E, 0) +
                 coalesce(age_9_11E, 0) + coalesce(age_12_14E, 0) +
                 coalesce(age_15_17E, 0)
    ) |>
    select(tract_geoid = GEOID, total_pop_acs = total_popE, pop_5_17)
  saveRDS(id_acs_age, id_b09001_cache)
}

# 3. Fetch & Cache Idaho 2020 Decennial Census Blocks (P1_001N & H1_001N) ----
id_blocks_cache <- file.path(cache_dir, "idaho_decennial_blocks_2020.rds")
if (file.exists(id_blocks_cache)) {
  id_blocks_raw <- readRDS(id_blocks_cache)
} else {
  message("Fetching Idaho 2020 Decennial Census Block populations & housing units...")
  id_blocks_raw <- get_decennial(
    geography = "block",
    variables = c(blk_pop = "P1_001N", blk_hu = "H1_001N"),
    state = "16",
    year = 2020,
    geometry = TRUE,
    output = "wide"
  ) |>
    # Convert block polygons to guaranteed-interior points in EPSG:5070 for fast, unambiguous point-in-polygon joins
    st_transform(5070) |>
    st_point_on_surface() |>
    mutate(tract_geoid = str_sub(GEOID, 1, 11)) |>
    select(block_geoid = GEOID, tract_geoid, blk_pop, blk_hu)
  saveRDS(id_blocks_raw, id_blocks_cache)
}

# 4. Prepare Projected Geometries (EPSG:5070) ----
tracts_5070 <- tracts_prod |>
  st_transform(5070) |>
  st_make_valid() |>
  left_join(id_acs_age, by = c("GEOID" = "tract_geoid")) |>
  mutate(
    total_pop         = coalesce(total_popE, 0),
    pop_5_17          = coalesce(pop_5_17, 0),
    tract_land_mi2    = set_units(st_area(geometry), "mi^2"),
    tract_pop_density = if_else(as.numeric(tract_land_mi2) > 0, as.numeric(total_pop / tract_land_mi2), 0),
    is_low_density    = tract_pop_density < 500
  ) |>
  select(tract_geoid = GEOID, total_pop, pop_5_17, tract_pop_density, is_low_density, tract_land_mi2)

districts_5070 <- districts_raw |>
  st_transform(5070) |>
  st_make_valid() |>
  select(GEOID, district_name = NAME) |>
  distinct(GEOID, .keep_all = TRUE)

# 5. Method A: Area-Weighted Apportionment (M0 & M1) ----
area_pieces <- suppressWarnings(st_intersection(tracts_5070, districts_5070)) |>
  mutate(
    piece_area_mi2 = set_units(st_area(geometry), "mi^2"),
    area_prop      = pmin(if_else(as.numeric(tract_land_mi2) > 0, as.numeric(piece_area_mi2 / tract_land_mi2), 0), 1.0)
  ) |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarize(
    m0_tot_pop   = sum(total_pop * area_prop, na.rm = TRUE),
    m0_rural_pop = sum((total_pop * area_prop)[is_low_density], na.rm = TRUE),
    m0_pct_rural = if_else(m0_tot_pop > 0, 100 * m0_rural_pop / m0_tot_pop, NA_real_),
    m1_pop_5_17  = sum(pop_5_17 * area_prop, na.rm = TRUE),
    m1_rural_5_17= sum((pop_5_17 * area_prop)[is_low_density], na.rm = TRUE),
    m1_pct_rural = if_else(m1_pop_5_17 > 0, 100 * m1_rural_5_17 / m1_pop_5_17, NA_real_),
    .groups = "drop"
  )

# 6. Method B: Census Block Dasymetric Apportionment (M2 & M3) ----
# Assign each block point to its enclosing Idaho school district
blocks_joined <- id_blocks_raw |>
  st_join(districts_5070, join = st_within, left = FALSE) |>
  st_drop_geometry()

# Compute within-tract block weights (fallback to housing units if a tract has 0 decennial pop)
block_weights <- blocks_joined |>
  group_by(tract_geoid) |>
  mutate(
    tract_blk_pop = sum(blk_pop, na.rm = TRUE),
    tract_blk_hu  = sum(blk_hu, na.rm = TRUE),
    blk_weight    = case_when(
      tract_blk_pop > 0 ~ blk_pop / tract_blk_pop,
      tract_blk_hu > 0  ~ blk_hu / tract_blk_hu,
      TRUE              ~ 1 / n()
    )
  ) |>
  ungroup() |>
  group_by(GEOID, tract_geoid) |>
  summarize(dasymetric_prop = sum(blk_weight, na.rm = TRUE), .groups = "drop")

dasymetric_pieces <- block_weights |>
  inner_join(tracts_5070 |> st_drop_geometry(), by = "tract_geoid") |>
  group_by(GEOID) |>
  summarize(
    m2_tot_pop    = sum(total_pop * dasymetric_prop, na.rm = TRUE),
    m2_rural_pop  = sum((total_pop * dasymetric_prop)[is_low_density], na.rm = TRUE),
    m2_pct_rural  = if_else(m2_tot_pop > 0, 100 * m2_rural_pop / m2_tot_pop, NA_real_),
    m3_pop_5_17   = sum(pop_5_17 * dasymetric_prop, na.rm = TRUE),
    m3_rural_5_17 = sum((pop_5_17 * dasymetric_prop)[is_low_density], na.rm = TRUE),
    m3_pct_rural  = if_else(m3_pop_5_17 > 0, 100 * m3_rural_5_17 / m3_pop_5_17, NA_real_),
    .groups = "drop"
  )

# 7. Merge with NCES CCD Enrollment & Locale Ground Truth ----
ccd_idaho <- nces_ccd_raw |>
  mutate(
    GEOID = str_pad(as.character(leaid), 7, pad = "0"),
    agency_type = as.character(agency_type),
    boundary_change_indicator = as.character(boundary_change_indicator),
    locale_major = case_when(
      str_starts(urban_centric_locale, "1") ~ "1: City",
      str_starts(urban_centric_locale, "2") ~ "2: Suburb",
      str_starts(urban_centric_locale, "3") ~ "3: Town",
      str_starts(urban_centric_locale, "4") ~ "4: Rural",
      TRUE ~ "Unknown"
    )
  ) |>
  filter(
    boundary_change_indicator %in% c("1", "2", "3", "4"),
    agency_type %in% c("1", "2", "7"),
    !is.na(enrollment),
    enrollment > 0
  ) |>
  select(GEOID, lea_name, locale_major, urban_centric_locale, ccd_enrollment = enrollment)

bench_df <- ccd_idaho |>
  left_join(area_pieces, by = "GEOID") |>
  left_join(dasymetric_pieces, by = "GEOID") |>
  mutate(
    # Scale M0/M2 total pop by statewide 5-17 share for an apples-to-apples enrollment scale check
    state_share_5_17 = sum(m1_pop_5_17, na.rm = TRUE) / sum(m0_tot_pop, na.rm = TRUE),
    m0_scaled_5_17   = m0_tot_pop * state_share_5_17,
    m2_scaled_5_17   = m2_tot_pop * state_share_5_17,
    diff_pct_rural_m3_vs_m0 = m3_pct_rural - m0_pct_rural,
    abs_shift_pct_rural     = abs(diff_pct_rural_m3_vs_m0)
  )

# 8. Compute Validation Metrics Against NCES CCD Enrollment ----
calc_metrics <- function(est, obs, model_label) {
  err     <- est - obs
  pct_err <- abs(err) / obs * 100
  tibble(
    Model             = model_label,
    N_Districts       = sum(!is.na(est) & !is.na(obs)),
    RMSE_Students     = round(sqrt(mean(err^2, na.rm = TRUE)), 1),
    MAE_Students      = round(mean(abs(err), na.rm = TRUE), 1),
    MAPE_Pct          = round(mean(pct_err, na.rm = TRUE), 2),
    Median_APE_Pct    = round(median(pct_err, na.rm = TRUE), 2),
    Total_Est_Sum     = round(sum(est, na.rm = TRUE), 0),
    Total_CCD_Enroll  = round(sum(obs, na.rm = TRUE), 0)
  )
}

metrics_table <- bind_rows(
  calc_metrics(bench_df$m0_scaled_5_17, bench_df$ccd_enrollment, "M0: Area-Weighted Total Pop (Scaled 5-17)"),
  calc_metrics(bench_df$m1_pop_5_17,    bench_df$ccd_enrollment, "M1: Area-Weighted ACS Ages 5-17"),
  calc_metrics(bench_df$m2_scaled_5_17, bench_df$ccd_enrollment, "M2: Block-Dasymetric Total Pop (Scaled 5-17)"),
  calc_metrics(bench_df$m3_pop_5_17,    bench_df$ccd_enrollment, "M3: Block-Dasymetric ACS Ages 5-17 (Full Ext #4)")
)

message("\n=== IDAHO ENROLLMENT ACCURACY BENCHMARK (vs. NCES CCD Enrollment) ===")
print(metrics_table, n = Inf)

# Check Rurality Alignment with Official NCES Urban-Centric Locale
locale_alignment <- bench_df |>
  group_by(locale_major) |>
  summarize(
    n_districts      = n(),
    mean_m0_rural    = round(mean(m0_pct_rural, na.rm = TRUE), 1),
    mean_m3_rural    = round(mean(m3_pct_rural, na.rm = TRUE), 1),
    mean_abs_shift   = round(mean(abs_shift_pct_rural, na.rm = TRUE), 1),
    .groups = "drop"
  )

message("\n=== % RURAL ALIGNMENT BY NCES LOCALE ===")
print(locale_alignment, n = Inf)

# Show Top 15 Idaho Districts Where % Rural Shifted Most
top_shifts <- bench_df |>
  st_drop_geometry() |>
  as_tibble() |>
  arrange(desc(abs_shift_pct_rural)) |>
  select(GEOID, lea_name, locale_major, ccd_enrollment, m0_pct_rural, m3_pct_rural, diff_pct_rural_m3_vs_m0) |>
  slice_head(n = 15)

message("\n=== TOP 15 IDAHO DISTRICTS BY % RURAL SHIFT (M3 - M0) ===")
print(as.data.frame(top_shifts), row.names = FALSE)

write_csv(bench_df, file.path(output_dir, paste0("idaho_rurality_benchmark_", year_this, ".csv")))
write_csv(metrics_table, file.path(output_dir, paste0("idaho_benchmark_summary_", year_this, ".csv")))
message("\nSaved benchmark CSVs to: ", output_dir)
