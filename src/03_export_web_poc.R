# 03_export_web_poc.R
# Export simplified GeoJSON with block-dasymetric ages 5-17 rurality estimates,
# percentile ranks, locale-mismatch flags, and guaranteed-interior label anchors.
# Author: Joe Champion, Boise State University

args <- commandArgs(trailingOnly = TRUE)
cache_dir <- if (length(args) >= 1) args[1] else "data_cache"
web_dir   <- if (length(args) >= 2) args[2] else "docs"
year_arg  <- if (length(args) >= 3 && args[3] != "" && toupper(args[3]) != "NA") as.integer(args[3]) else NA_integer_

library(tidyverse)
library(sf)

sf_use_s2(FALSE)

# Resolve target year
manifest_path <- file.path(cache_dir, "latest_cached_year.txt")
year_this <- if (!is.na(year_arg)) {
  year_arg
} else if (file.exists(manifest_path)) {
  as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE)))
} else {
  stop("No year specified and ", manifest_path, " not found.")
}

checkpoint_path <- file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))
if (!file.exists(checkpoint_path)) {
  stop("Missing spatial checkpoint: ", checkpoint_path)
}

script_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir  <- if (length(script_flag) > 0) dirname(sub("^--file=", "", script_flag[1])) else "src"
source(file.path(script_dir, "utils", "district_name_helpers.R"))

message("Loading spatial checkpoint for web export (", year_this, ")...")
districts_sf <- readRDS(checkpoint_path) |>
  filter(!st_is_empty(geometry)) |>
  st_transform(5070) |>
  st_make_valid()

# Compute guaranteed-interior label anchors in EPSG:5070 prior to simplification
anchors_wgs84 <- suppressWarnings(st_point_on_surface(districts_sf)) |>
  st_transform(4326) |>
  st_coordinates()

# Compute within-state and national percentile ranks + NCES locale mismatch flags
districts_enriched <- districts_sf |>
  mutate(
    label_lon  = round(anchors_wgs84[, "X"], 5),
    label_lat  = round(anchors_wgs84[, "Y"], 5),
    short_name = clean_district_name(lea_name, district_name),
    short_name = if_else(
      is.na(short_name) | str_trim(short_name) == "" | str_to_lower(str_trim(short_name)) %in% c("school", "district"),
      coalesce(district_name, lea_name),
      short_name
    ),
    pct_pop_rural         = round(pct_pop_rural, 1),
    est_total_pop_5_17    = round(est_total_pop_5_17, 0),
    est_rural_pop_5_17    = round(est_rural_pop_5_17, 0),
    est_total_pop         = round(est_total_pop, 0),
    weighted_mean_density = round(weighted_mean_density, 1),
    area_mi2              = round(as.numeric(district_area_mi2), 1),
    nat_rural_pctl        = if_else(!is.na(pct_pop_rural), round(100 * percent_rank(pct_pop_rural), 0), NA_real_)
  ) |>
  group_by(state_abbr) |>
  mutate(
    state_rural_pctl = if_else(!is.na(pct_pop_rural), round(100 * percent_rank(pct_pop_rural), 0), NA_real_)
  ) |>
  ungroup() |>
  mutate(
    locale_major_code = str_sub(as.character(urban_centric_locale), 1, 1),
    locale_mismatch = case_when(
      is.na(pct_pop_rural) | is.na(locale_major_code) ~ FALSE,
      locale_major_code %in% c("1", "2") & pct_pop_rural >= 15 ~ TRUE, # City/Suburb serving >=15% rural youth
      locale_major_code == "3" & pct_pop_rural >= 50 ~ TRUE,           # Town serving majority-rural youth
      locale_major_code == "4" & pct_pop_rural < 50 ~ TRUE,            # Rural locale with <50% rural youth
      TRUE ~ FALSE
    ),
    mismatch_reason = case_when(
      !locale_mismatch ~ NA_character_,
      locale_major_code == "1" & pct_pop_rural >= 15 ~ "NCES City locale serving ≥15% rural youth",
      locale_major_code == "2" & pct_pop_rural >= 15 ~ "NCES Suburb locale serving ≥15% rural youth",
      locale_major_code == "3" & pct_pop_rural >= 50 ~ "NCES Town locale serving majority-rural (≥50%) youth",
      locale_major_code == "4" & pct_pop_rural < 50  ~ "NCES Rural locale with <50% youth in low-density tracts",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    GEOID,
    state_abbr,
    state_name,
    lea_name,
    short_name,
    urban_centric_locale,
    urban_centric_locale_type,
    enrollment,
    number_of_schools,
    pct_pop_rural,
    est_total_pop_5_17,
    est_rural_pop_5_17,
    est_total_pop,
    weighted_mean_density,
    area_mi2,
    state_rural_pctl,
    nat_rural_pctl,
    locale_mismatch,
    mismatch_reason,
    label_lon,
    label_lat,
    geometry
  )

# Simplify geometries in EPSG:5070 (dTolerance = 500m) and transform to WGS84
message("Simplifying district geometries for fast web rendering...")
districts_web <- districts_enriched |>
  st_simplify(preserveTopology = TRUE, dTolerance = 500) |>
  st_transform(4326)

out_dir <- file.path(web_dir, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
geojson_path <- file.path(out_dir, "districts_poc.geojson")

if (file.exists(geojson_path)) file.remove(geojson_path)
st_write(districts_web, geojson_path, driver = "GeoJSON", quiet = TRUE)

# Write lightweight metadata manifest for the web frontend
meta_path <- file.path(out_dir, "rurality_meta.json")
jsonlite::write_json(
  list(
    data_year   = year_this,
    methodology = "Block-dasymetric ACS ages 5-17 share in low-density census tracts (< 500 people/sq mi)",
    updated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ),
  meta_path,
  auto_unbox = TRUE,
  pretty = TRUE
)

message("Exported ", nrow(districts_web), " districts to: ", geojson_path)