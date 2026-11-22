# 03_export_web_poc.R
# Export simplified WGS84 GeoJSON for the static interactive web map (docs/data/districts_poc.geojson)
# Author: Joe Champion, Boise State University

library(sf)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
cache_dir <- if (length(args) >= 1) args[1] else "data_cache"
web_dir   <- if (length(args) >= 2) args[2] else "docs/data"

rds_file <- list.files(cache_dir, pattern = "^districts_complete_sf_.*\\.rds$", full.names = TRUE)[1]
stopifnot("No districts_complete_sf_*.rds found in cache_dir" = !is.na(rds_file))

districts_sf <- readRDS(rds_file)

web_districts <- districts_sf |>
  filter(!st_is_empty(geometry)) |>
  arrange(desc(as.numeric(district_area_mi2))) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 500) |>
  st_make_valid() |>
  st_transform(4326) |>
  transmute(
    geoid       = GEOID,
    name        = district_name,
    state       = state_abbr,
    pct_rural   = round(pct_pop_rural, 1),
    total_pop   = round(est_total_pop, 0),
    rural_pop   = round(est_rural_pop, 0),
    density     = round(weighted_mean_density, 1),
    area_mi2    = round(as.numeric(district_area_mi2), 1),
    locale      = urban_centric_locale_type,
    enrollment  = enrollment,
    schools     = number_of_schools
  )

dir.create(web_dir, recursive = TRUE, showWarnings = FALSE)
out_geojson <- file.path(web_dir, "districts_poc.geojson")
if (file.exists(out_geojson)) file.remove(out_geojson)

st_write(web_districts, out_geojson, driver = "GeoJSON", quiet = TRUE)
message("Exported ", nrow(web_districts), " districts -> ", out_geojson,
        " (", round(file.size(out_geojson) / 1e6, 2), " MB)")