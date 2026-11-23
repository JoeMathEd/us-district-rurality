# src/benchmark_idaho_ext3.R
# Prototype & benchmark Extension #3 (RUCA, Census Urban Areas, Distance to Urban Core)
# across Idaho's 115 bounded school districts before national deployment.

library(tidyverse)
library(sf)
library(tigris)
library(units)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

cache_dir <- "/bsuscratch/joechampion/district-rurality/data_cache"
year_this <- as.integer(trimws(readLines(file.path(cache_dir, "latest_cached_year.txt"), n = 1)))

# 1. Load cached spatial checkpoint, tract pops, and 2020 block points ----
districts_sf <- readRDS(file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))) |>
  filter(state_fip == "16", !st_is_empty(geometry)) |>
  st_transform(5070)

tract_pops <- readRDS(file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))) |>
  filter(stringr::str_sub(GEOID, 1, 2) == "16") |>
  st_drop_geometry() |>
  transmute(
    tract_geoid = GEOID,
    pop_5_17 = coalesce(age_5E, 0) + coalesce(age_6_8E, 0) +
      coalesce(age_9_11E, 0) + coalesce(age_12_14E, 0) +
      coalesce(age_15_17E, 0)
  )

id_blocks <- readRDS(file.path(cache_dir, "decennial_block_points_2020.rds")) |>
  filter(state_fip == "16") |>
  st_transform(5070)

# 2. Fetch & cache Census Urban Areas (national file, filtered to ID + 150-mile buffer for cross-border cores like Spokane/Salt Lake) ----
ua_cache_path <- file.path(cache_dir, paste0("urban_areas_raw_", year_this, ".rds"))
if (file.exists(ua_cache_path)) {
  ua_raw <- readRDS(ua_cache_path)
} else {
  message("Downloading TIGER/Line Urban Areas for ", year_this, "...")
  ua_raw <- tigris::urban_areas(year = year_this, cb = FALSE)
  saveRDS(ua_raw, ua_cache_path)
}

ua_5070 <- ua_raw |>
  st_transform(5070) |>
  st_make_valid()

# Keep Urban Areas within 200 miles of Idaho (captures Spokane WA, Logan/Ogden UT, Ontario OR, Missoula MT)
id_buffer <- st_buffer(st_union(districts_sf), dist = set_units(200, "mi"))
ua_region <- ua_5070[lengths(st_intersects(ua_5070, id_buffer)) > 0, ]

# Note: 2020 Census Urban Areas use UATYP10/NAMELSAD20; population is available in Decennial/ACS or via area/2010-2020 crosswalk.
# Fetch 2020 Decennial Urban Area populations (P1_001N) to distinguish Urbanized Areas (>=50k) from Urban Clusters (5k-50k)
ua_pop_cache <- file.path(cache_dir, "urban_area_pops_2020.rds")
if (file.exists(ua_pop_cache)) {
  ua_pops <- readRDS(ua_pop_cache)
} else {
  ua_pops <- tidycensus::get_decennial(
    geography = "urban area",
    variables = "P1_001N",
    year = 2020
  ) |>
    transmute(UACE20 = GEOID, ua_pop_2020 = value)
  saveRDS(ua_pops, ua_pop_cache)
}

ua_region <- ua_region |>
  rename(UACE20 = any_of(c("UACE20", "UACE10", "GEOID20", "GEOID"))) |>
  left_join(ua_pops, by = "UACE20")

ua_50k <- ua_region |> filter(coalesce(ua_pop_2020, 0) >= 50000)

# 3. Fetch & cache USDA ERS Tract-Level RUCA Codes (2020 or 2010 legacy fallback) ----
ruca_cache_path <- file.path(cache_dir, "ruca_tracts_cached.rds")
if (file.exists(ruca_cache_path)) {
  ruca_df <- readRDS(ruca_cache_path)
} else {
  message("Downloading USDA ERS RUCA tract codes...")
  # USDA ERS 2020 RUCA CSV endpoint (falls back to 2010 revised CSV if needed)
  ruca_url_2020 <- "https://www.ers.usda.gov/media/5346/ruca2020tract.csv"
  ruca_raw <- tryCatch(
    readr::read_csv(ruca_url_2020, show_col_types = FALSE),
    error = function(e) readr::read_csv("https://www.ers.usda.gov/media/5351/ruca2010revised.csv", show_col_types = FALSE)
  )
  # Normalize column names across 2020 and 2010 ERS schemas
  geoid_col <- names(ruca_raw)[stringr::str_detect(tolower(names(ruca_raw)), "tract.*fips|fips.*tract|geoid")][1]
  prim_col  <- names(ruca_raw)[stringr::str_detect(tolower(names(ruca_raw)), "primary.*ruca|ruca1")][1]
  ruca_df <- ruca_raw |>
    transmute(
      tract_geoid = stringr::str_pad(as.character(.data[[geoid_col]]), 11, pad = "0"),
      ruca_primary = as.integer(.data[[prim_col]])
    ) |>
    distinct(tract_geoid, .keep_all = TRUE)
  saveRDS(ruca_df, ruca_cache_path)
}

# 4. Compute Block-Level Urban Area Membership & District Apportionments ----
id_blocks_aug <- id_blocks |>
  mutate(
    in_urban_area = lengths(st_intersects(geometry, ua_region)) > 0
  )

tract_denoms <- id_blocks_aug |>
  st_drop_geometry() |>
  group_by(tract_geoid) |>
  summarize(
    tract_blk_pop = sum(blk_pop, na.rm = TRUE),
    tract_blk_hu  = sum(blk_hu, na.rm = TRUE),
    tract_n_blks  = n(),
    .groups = "drop"
  )

blk_joined <- st_join(
  id_blocks_aug,
  districts_sf |> select(GEOID),
  join = st_intersects,
  left = FALSE
)

# 4A. Block-dasymetric youth shares by RUCA & Urban Area status
ext3_youth <- blk_joined |>
  st_drop_geometry() |>
  left_join(tract_denoms, by = "tract_geoid") |>
  mutate(
    blk_weight = case_when(
      tract_blk_pop > 0 ~ blk_pop / tract_blk_pop,
      tract_blk_hu > 0  ~ blk_hu / tract_blk_hu,
      tract_n_blks > 0  ~ 1 / tract_n_blks,
      TRUE              ~ 0
    )
  ) |>
  group_by(GEOID, tract_geoid, in_urban_area) |>
  summarize(piece_prop = pmin(sum(blk_weight, na.rm = TRUE), 1.0), .groups = "drop") |>
  inner_join(tract_pops, by = "tract_geoid") |>
  left_join(ruca_df |> filter(stringr::str_sub(tract_geoid, 1, 2) == "16"), by = "tract_geoid") |>
  mutate(
    est_5_17 = pop_5_17 * piece_prop,
    is_ruca_rural = ruca_primary >= 7 & ruca_primary <= 10,
    is_ruca_micro_or_rural = ruca_primary >= 4 & ruca_primary <= 10
  ) |>
  group_by(GEOID) |>
  summarize(
    tot_5_17_check         = sum(est_5_17, na.rm = TRUE),
    pct_youth_outside_ua   = if_else(tot_5_17_check > 0, 100 * sum(est_5_17[!in_urban_area], na.rm = TRUE) / tot_5_17_check, NA_real_),
    pct_youth_ruca_rural   = if_else(tot_5_17_check > 0, 100 * sum(est_5_17[coalesce(is_ruca_rural, FALSE)], na.rm = TRUE) / tot_5_17_check, NA_real_),
    pct_youth_ruca_nonmetro = if_else(tot_5_17_check > 0, 100 * sum(est_5_17[coalesce(is_ruca_micro_or_rural, FALSE)], na.rm = TRUE) / tot_5_17_check, NA_real_),
    .groups = "drop"
  )

# 4B. Population-weighted district centroids -> Distance to nearest Urban Area & >=50k Urbanized Area
pop_centroids <- blk_joined |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2],
    w = if_else(blk_pop > 0, as.numeric(blk_pop), 1e-3)
  ) |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarize(
    cx = weighted.mean(x, w, na.rm = TRUE),
    cy = weighted.mean(y, w, na.rm = TRUE),
    .groups = "drop"
  ) |>
  st_as_sf(coords = c("cx", "cy"), crs = 5070)

dist_any_ua_m <- apply(st_distance(pop_centroids, ua_region), 1, min)
dist_50k_ua_m <- apply(st_distance(pop_centroids, ua_50k), 1, min)

pop_centroids <- pop_centroids |>
  st_drop_geometry() |>
  mutate(
    dist_any_ua_mi = as.numeric(set_units(set_units(dist_any_ua_m, "m"), "mi")),
    dist_50k_ua_mi = as.numeric(set_units(set_units(dist_50k_ua_m, "m"), "mi"))
  )

# 5. Merge & Benchmark Comparisons Across Idaho's 115 Bounded Districts ----
id_bench <- districts_sf |>
  st_drop_geometry() |>
  left_join(ext3_youth, by = "GEOID") |>
  left_join(pop_centroids, by = "GEOID") |>
  mutate(across(where(~ inherits(.x, "units")), as.numeric))

cat("\n=== IDAHO EXTENSION #3 SUMMARY BY NCES LOCALE TYPE ===\n")
id_bench |>
  group_by(urban_centric_locale_type) |>
  summarize(
    n_districts           = n(),
    mean_tract_rural_pct  = round(mean(pct_pop_rural, na.rm = TRUE), 1),
    mean_outside_ua_pct   = round(mean(pct_youth_outside_ua, na.rm = TRUE), 1),
    mean_ruca_rural_pct   = round(mean(pct_youth_ruca_rural, na.rm = TRUE), 1),
    med_dist_any_ua_mi    = round(median(dist_any_ua_mi, na.rm = TRUE), 1),
    med_dist_50k_ua_mi    = round(median(dist_50k_ua_mi, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  print(n = 20)

cat("\n=== CORRELATION MATRIX ACROSS RURALITY INDICATORS (IDAHO, n=115) ===\n")
id_bench |>
  select(pct_pop_rural, pct_youth_outside_ua, pct_youth_ruca_rural, pct_youth_ruca_nonmetro, dist_any_ua_mi, dist_50k_ua_mi) |>
  cor(use = "pairwise.complete.obs") |>
  round(3) |>
  print()

cat("\n=== TOP 15 DIVERGENCES: TRACT DENSITY (<500/mi2) VS. OUTSIDE URBAN AREA ===\n")
id_bench |>
  mutate(diff_tract_vs_ua = round(pct_pop_rural - pct_youth_outside_ua, 1)) |>
  arrange(desc(abs(diff_tract_vs_ua))) |>
  select(GEOID, lea_name, urban_centric_locale_type, pct_pop_rural, pct_youth_outside_ua, pct_youth_ruca_rural, dist_50k_ua_mi, diff_tract_vs_ua) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  head(15) |>
  print(n = 15)

saveRDS(id_bench, file.path(cache_dir, "idaho_ext3_benchmark.rds"))