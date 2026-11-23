# 02a_compute_spatial_metrics.R
# Compute block-dasymetric tract-to-district school-aged (5-17) rurality estimates,
# Extension #3 indicators (Census Urban Areas, USDA ERS 2020 RUCA, distance to urban core),
# merge NCES CCD directory metadata, and save spatial + tabular checkpoints.
# Author: Joe Champion, Boise State University

args <- commandArgs(trailingOnly = TRUE)
cache_dir  <- if (length(args) >= 1) args[1] else "data_cache"
output_dir <- if (length(args) >= 2) args[2] else "outputs"
year_arg   <- if (length(args) >= 3 && args[3] != "" && toupper(args[3]) != "NA") as.integer(args[3]) else NA_integer_

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Configuration ----
library(tidyverse)
library(sf)
library(tidycensus)
library(units)
library(furrr)

sf_use_s2(FALSE)

n_workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
spatial_workers <- min(n_workers, 6L)
if (!is.na(spatial_workers) && spatial_workers > 1L) {
  plan(multicore, workers = spatial_workers)
} else {
  plan(sequential)
}
message("Configured 02a spatial computation with ", spatial_workers, " worker(s).")

manifest_path <- file.path(cache_dir, "latest_cached_year.txt")
year_this <- if (!is.na(year_arg)) {
  year_arg
} else if (file.exists(manifest_path)) {
  as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE)))
} else {
  stop("No year specified and ", manifest_path, " not found. Run 01_fetch_rurality_data.R first.")
}

message("Computing national block-dasymetric & Extension #3 spatial metrics for year: ", year_this)

fips_lookup <- tidycensus::fips_codes |>
  select(state_fip = state_code, state_abbr = state, state_name) |>
  distinct()

# 1. Load Cached Raw Datasets ----
tracts_path      <- file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))
districts_path   <- file.path(cache_dir, paste0("districts_geo_raw_", year_this, ".rds"))
nces_path        <- file.path(cache_dir, paste0("nces_ccd_raw_", year_this, ".rds"))
blocks_path      <- file.path(cache_dir, "decennial_block_points_2020.rds")
urban_areas_path <- file.path(cache_dir, paste0("urban_areas_raw_", year_this, ".rds"))
ruca_path        <- file.path(cache_dir, "ruca_tracts_2020.rds")

required_files <- c(tracts_path, districts_path, nces_path, blocks_path, urban_areas_path, ruca_path)
if (!all(file.exists(required_files))) {
  stop("Missing required cached .rds files in ", cache_dir, ". Run 01_fetch_rurality_data.R first.")
}

tract_pops_raw         <- readRDS(tracts_path)
districts_geo_raw      <- readRDS(districts_path)
nces_ccd_raw           <- readRDS(nces_path)
decennial_block_points <- readRDS(blocks_path) |> st_transform(5070)
urban_areas_raw        <- readRDS(urban_areas_path)
ruca_df                <- readRDS(ruca_path)

# 2. Project & Wrangle Census Tracts + Merge 2020 RUCA Codes (EPSG:5070) ----
tract_pops_clean <- tract_pops_raw |>
  st_transform(5070) |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    total_pop         = coalesce(total_popE, 0),
    pop_5_17          = coalesce(age_5E, 0) + coalesce(age_6_8E, 0) +
                        coalesce(age_9_11E, 0) + coalesce(age_12_14E, 0) +
                        coalesce(age_15_17E, 0),
    tract_land_mi2    = set_units(st_area(geometry), "mi^2"),
    tract_pop_density = if_else(
      as.numeric(tract_land_mi2) > 0,
      as.numeric(total_pop / tract_land_mi2),
      0
    ),
    is_low_density    = tract_pop_density < 500,
    state_fip         = stringr::str_sub(GEOID, 1, 2)
  ) |>
  st_drop_geometry() |>
  select(
    tract_geoid = GEOID,
    state_fip,
    total_pop,
    pop_5_17,
    tract_pop_density,
    is_low_density
  ) |>
  left_join(ruca_df, by = "tract_geoid") |>
  mutate(
    is_ruca_rural    = coalesce(ruca_primary >= 7L & ruca_primary <= 10L, FALSE),
    is_ruca_nonmetro = coalesce(ruca_primary >= 4L & ruca_primary <= 10L, FALSE)
  )

# Fix CT (state_fip == "09") 2020 Decennial block tract_geoids (legacy counties 09001-09015)
# to match 2024 ACS Planning Region tract_geoids (09110-09190) via spatial nearest-tract join
ct_block_idx <- which(decennial_block_points$state_fip == "09")
if (length(ct_block_idx) > 0) {
  message("Re-indexing ", length(ct_block_idx), " Connecticut 2020 blocks to 2024 Planning Region tracts...")
  ct_tracts_2024 <- tract_pops_raw |>
    filter(stringr::str_sub(GEOID, 1, 2) == "09") |>
    st_transform(5070) |>
    select(new_tract_geoid = GEOID)

  ct_reindexed <- st_join(
    decennial_block_points[ct_block_idx, ] |> select(-tract_geoid),
    ct_tracts_2024,
    join = st_nearest_feature
  ) |>
    rename(tract_geoid = new_tract_geoid)

  decennial_block_points$tract_geoid[ct_block_idx] <- ct_reindexed$tract_geoid
  rm(ct_tracts_2024, ct_reindexed)
}

rm(tract_pops_raw, ruca_df)
gc()

# 3. Classify Block Points by Census Urban Area Membership & Identify >=50k Urbanized Areas ----
message("Classifying 2020 block points against Census Urban Areas (Extension #3)...")
ua_id_col <- intersect(c("UACE", "UACE20", "UACE10", "GEOID", "GEOID20"), names(urban_areas_raw))[1]
if (is.na(ua_id_col)) {
  stop("Could not find Urban Area ID column in urban_areas_raw. Columns: ", paste(names(urban_areas_raw), collapse = ", "))
}

ua_5070 <- urban_areas_raw |>
  st_transform(5070) |>
  st_make_valid() |>
  rename(UACE = all_of(ua_id_col)) |>
  select(UACE)

rm(urban_areas_raw)
gc()

# Assign every block point to its enclosing Urban Area (NA if outside all Census Urban Areas)
ua_idx_per_block <- st_intersects(decennial_block_points, ua_5070)
first_ua_idx <- vapply(ua_idx_per_block, \(x) if (length(x) > 0L) x[1L] else NA_integer_, integer(1))

decennial_block_points <- decennial_block_points |>
  mutate(
    uace          = ua_5070$UACE[first_ua_idx],
    in_urban_area = !is.na(uace)
  )

# Compute exact 2020 Urban Area populations from block points to isolate >=50,000 Urbanized Areas
ua_pops <- decennial_block_points |>
  st_drop_geometry() |>
  filter(in_urban_area) |>
  group_by(uace) |>
  summarize(ua_pop_2020 = sum(blk_pop, na.rm = TRUE), .groups = "drop")

ua_50k <- ua_5070 |>
  inner_join(ua_pops |> filter(ua_pop_2020 >= 50000), by = c("UACE" = "uace"))

rm(ua_idx_per_block, first_ua_idx, ua_pops)
gc()

# 4. Project & Wrangle School District Boundaries ----
if (!"tiger_district_type" %in% names(districts_geo_raw)) {
  districts_geo_raw <- districts_geo_raw |>
    mutate(tiger_district_type = NA_character_)
}

districts_geo <- districts_geo_raw |>
  st_transform(5070) |>
  st_make_valid() |>
  select(GEOID, state_fip = STATEFP, district_name = NAME, tiger_district_type) |>
  filter(as.numeric(state_fip) < 60) |>
  filter(!st_is_empty(geometry)) |>
  distinct(GEOID, .keep_all = TRUE) |>
  mutate(district_area_mi2 = set_units(st_area(geometry), "mi^2"))

rm(districts_geo_raw)
gc()

districts_clean <- districts_geo |>
  select(GEOID, state_fip, district_name, district_area_mi2)

# 5. State-by-State Block-Dasymetric & Extension #3 Apportionments ----
message("Computing state-by-state block-dasymetric apportionments & urban core distances...")

blocks_by_state    <- split(decennial_block_points, decennial_block_points$state_fip)
districts_by_state <- split(districts_clean |> select(GEOID, district_name), districts_clean$state_fip)

rm(decennial_block_points)
gc()

state_fips <- names(districts_by_state)
blk_counts <- vapply(state_fips, \(f) if (!is.null(blocks_by_state[[f]])) nrow(blocks_by_state[[f]]) else 0L, integer(1))
state_fips <- state_fips[order(blk_counts, decreasing = TRUE)]

# Pre-filter regional Urban Areas within 250 miles of each state so st_distance() stays fast
state_tasks <- map(state_fips, \(fip) {
  dist_sub <- districts_by_state[[fip]]
  st_buf   <- st_buffer(st_union(dist_sub), dist = set_units(250, "mi"))
  list(
    fip       = fip,
    blk       = blocks_by_state[[fip]],
    dist      = dist_sub,
    ua_any    = ua_5070[lengths(st_intersects(ua_5070, st_buf)) > 0, ],
    ua_50k_st = ua_50k[lengths(st_intersects(ua_50k, st_buf)) > 0, ]
  )
})

rm(blocks_by_state, districts_by_state, ua_5070, ua_50k)
gc()

state_results <- future_map(
  state_tasks,
  \(task) {
    blk_sub  <- task$blk
    dist_sub <- task$dist

    if (is.null(blk_sub) || is.null(dist_sub) || nrow(blk_sub) == 0 || nrow(dist_sub) == 0) return(NULL)

    joined_sf <- st_join(blk_sub, dist_sub, join = st_intersects, left = FALSE)
    if (nrow(joined_sf) == 0) return(NULL)

    # 5A. Population-weighted district centroids -> distance to nearest Urban Area & >=50k Urbanized Area
    coords <- st_coordinates(joined_sf)
    pop_centroids <- joined_sf |>
      st_drop_geometry() |>
      mutate(
        x = coords[, 1],
        y = coords[, 2],
        w = if_else(blk_pop > 0, as.numeric(blk_pop), 1e-3)
      ) |>
      group_by(GEOID) |>
      summarize(
        cx = weighted.mean(x, w, na.rm = TRUE),
        cy = weighted.mean(y, w, na.rm = TRUE),
        .groups = "drop"
      ) |>
      st_as_sf(coords = c("cx", "cy"), crs = 5070)

    dist_any_m <- if (nrow(task$ua_any) > 0) apply(st_distance(pop_centroids, task$ua_any), 1, min) else rep(NA_real_, nrow(pop_centroids))
    dist_50k_m <- if (nrow(task$ua_50k_st) > 0) apply(st_distance(pop_centroids, task$ua_50k_st), 1, min) else rep(NA_real_, nrow(pop_centroids))

    dist_metrics <- pop_centroids |>
      st_drop_geometry() |>
      mutate(
        dist_any_ua_mi = as.numeric(set_units(set_units(dist_any_m, "m"), "mi")),
        dist_50k_ua_mi = as.numeric(set_units(set_units(dist_50k_m, "m"), "mi"))
      )

    # 5B. Tract denominators & block-dasymetric tract-to-district-by-urban-area pieces
    tract_denoms <- blk_sub |>
      st_drop_geometry() |>
      group_by(tract_geoid) |>
      summarize(
        tract_blk_pop = sum(blk_pop, na.rm = TRUE),
        tract_blk_hu  = sum(blk_hu, na.rm = TRUE),
        tract_n_blks  = n(),
        .groups = "drop"
      )

    pieces <- joined_sf |>
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
      group_by(GEOID, district_name, tract_geoid, in_urban_area) |>
      summarize(dasymetric_prop = pmin(sum(blk_weight, na.rm = TRUE), 1.0), .groups = "drop")

    message("  [", task$fip, "] Apportioned ", nrow(blk_sub), " blocks -> ", nrow(pieces), " tract-district pieces")
    flush.console()

    list(pieces = pieces, dist_metrics = dist_metrics)
  },
  .options = furrr_options(seed = TRUE, globals = FALSE)
) |>
  compact()

rm(state_tasks)
gc()

dasymetric_pieces <- map(state_results, "pieces") |>
  bind_rows() |>
  inner_join(tract_pops_clean, by = "tract_geoid") |>
  mutate(
    est_piece_pop_5_17 = pop_5_17 * dasymetric_prop,
    est_piece_pop      = total_pop * dasymetric_prop
  )

district_distances <- map(state_results, "dist_metrics") |>
  bind_rows() |>
  distinct(GEOID, .keep_all = TRUE)

rm(state_results, tract_pops_clean)
gc()

# 6. Summarize District-Level Rural Serving & Extension #3 Metrics ----
districts_rural_summary <- dasymetric_pieces |>
  group_by(GEOID, district_name) |>
  summarize(
    n_intersecting_tracts   = n_distinct(tract_geoid),
    est_total_pop_5_17      = sum(est_piece_pop_5_17, na.rm = TRUE),
    est_rural_pop_5_17      = sum(est_piece_pop_5_17[is_low_density], na.rm = TRUE),
    pct_pop_rural           = if_else(est_total_pop_5_17 > 0, 100 * (est_rural_pop_5_17 / est_total_pop_5_17), NA_real_),
    pct_youth_outside_ua    = if_else(est_total_pop_5_17 > 0, 100 * (sum(est_piece_pop_5_17[!in_urban_area], na.rm = TRUE) / est_total_pop_5_17), NA_real_),
    pct_youth_ruca_rural    = if_else(est_total_pop_5_17 > 0, 100 * (sum(est_piece_pop_5_17[is_ruca_rural], na.rm = TRUE) / est_total_pop_5_17), NA_real_),
    pct_youth_ruca_nonmetro = if_else(est_total_pop_5_17 > 0, 100 * (sum(est_piece_pop_5_17[is_ruca_nonmetro], na.rm = TRUE) / est_total_pop_5_17), NA_real_),
    est_total_pop           = sum(est_piece_pop, na.rm = TRUE),
    est_rural_pop           = sum(est_piece_pop[is_low_density], na.rm = TRUE),
    weighted_mean_density   = if_else(
      est_total_pop_5_17 > 0,
      weighted.mean(tract_pop_density, est_piece_pop_5_17, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  left_join(district_distances, by = "GEOID") |>
  right_join(
    districts_geo |> st_drop_geometry() |> select(GEOID, state_fip, district_area_mi2, tiger_district_type),
    by = "GEOID"
  )

# 7. Clean & Merge NCES CCD Directory Data ----
nces_ccd_cleaned <- nces_ccd_raw |>
  mutate(
    GEOID = stringr::str_pad(as.character(leaid), 7, pad = "0"),
    boundary_change_indicator = as.character(boundary_change_indicator),
    agency_type = as.character(agency_type),
    urban_centric_locale_type = case_when(
      str_starts(urban_centric_locale, "1") ~ "1: City",
      str_starts(urban_centric_locale, "2") ~ "2: Suburb",
      str_starts(urban_centric_locale, "3") ~ "3: Town",
      str_starts(urban_centric_locale, "4") ~ "4: Rural",
      TRUE ~ NA_character_
    ),
    is_operational = boundary_change_indicator %in% c("1", "2", "3", "4"),
    operational_desc = case_when(
      boundary_change_indicator == "1" ~ "Open (No change)",
      boundary_change_indicator == "2" ~ "Open (Boundary change)",
      boundary_change_indicator == "3" ~ "New district",
      boundary_change_indicator == "4" ~ "Added",
      boundary_change_indicator == "5" ~ "Closed",
      boundary_change_indicator == "6" ~ "Inactive",
      boundary_change_indicator == "7" ~ "Future",
      TRUE ~ "Unknown"
    ),
    agency_type_desc = case_when(
      agency_type == "1" ~ "Regular local school district (independent)",
      agency_type == "2" ~ "Local school district (supervisory union component)",
      agency_type == "3" ~ "Supervisory union administrative center",
      agency_type == "4" ~ "Regional Education Service Agency (RESA/BOCES/ESD)",
      agency_type == "5" ~ "State-operated agency",
      agency_type == "6" ~ "Federally operated agency (e.g., BIE)",
      agency_type == "7" ~ "Independent charter district",
      agency_type == "8" ~ "Other education agency",
      agency_type == "9" ~ "Specialized public school district",
      TRUE ~ "Other/Unknown"
    ),
    is_traditional_district = agency_type %in% c("1", "2", "7")
  ) |>
  select(
    GEOID,
    lea_name,
    agency_type,
    agency_type_desc,
    is_traditional_district,
    boundary_change_indicator,
    operational_desc,
    is_operational,
    urban_centric_locale,
    urban_centric_locale_type,
    number_of_schools,
    enrollment,
    teachers_total_fte
  )

districts_complete <- districts_rural_summary |>
  left_join(nces_ccd_cleaned, by = "GEOID") |>
  filter(
    is.na(is_operational) | is_operational,
    is.na(is_traditional_district) | is_traditional_district,
    is.na(enrollment) | enrollment >= 0
  ) |>
  mutate(
    district_name           = coalesce(district_name, lea_name),
    lea_name                = coalesce(lea_name, district_name),
    pct_pop_rural           = if_else(is.na(enrollment) | enrollment <= 0, NA_real_, pct_pop_rural),
    pct_youth_outside_ua    = if_else(is.na(enrollment) | enrollment <= 0, NA_real_, pct_youth_outside_ua),
    pct_youth_ruca_rural    = if_else(is.na(enrollment) | enrollment <= 0, NA_real_, pct_youth_ruca_rural),
    pct_youth_ruca_nonmetro = if_else(is.na(enrollment) | enrollment <= 0, NA_real_, pct_youth_ruca_nonmetro)
  )

districts_complete_sf <- districts_complete |>
  left_join(fips_lookup, by = "state_fip") |>
  left_join(districts_geo |> select(GEOID, geometry), by = "GEOID") |>
  arrange(desc(district_area_mi2)) |>
  st_as_sf()

spatial_checkpoint_path <- file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))
saveRDS(districts_complete_sf, spatial_checkpoint_path)
message("Saved spatial checkpoint (with Extension #3 metrics) to: ", spatial_checkpoint_path)

# 8. Export Tabular Summary ----
districts_tabular <- districts_complete_sf |>
  st_drop_geometry() |>
  relocate(GEOID, state_abbr, state_name, lea_name, .before = 1) |>
  mutate(across(where(~ inherits(.x, "units")), as.numeric))

csv_out_path <- file.path(output_dir, paste0("US_School_Districts_Rural_Summary_", year_this, ".csv"))
rds_out_path <- file.path(output_dir, paste0("US_School_Districts_Rural_Summary_", year_this, ".rds"))

write_csv(districts_tabular, csv_out_path)
saveRDS(districts_tabular, rds_out_path)
plan(sequential)
message("Saved tabular summaries to: ", csv_out_path, " and ", rds_out_path)