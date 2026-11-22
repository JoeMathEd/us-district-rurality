# 01_fetch_rurality_data.R
# Determine the most recent common year across ACS, TIGER/Line, and NCES CCD,
# and fetch/cache raw datasets only if that year is not already cached.
# Author: Joe Champion, Boise State University

args <- commandArgs(trailingOnly = TRUE)
cache_dir  <- if (length(args) >= 1) args[1] else "data_cache"
force_sync <- if (length(args) >= 2) as.logical(args[2]) else FALSE

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Configuration ----
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(educationdata)

options(tigris_use_cache = TRUE)
options(tigris_cache_dir = cache_dir)
sf_use_s2(TRUE)

# 1. Probe Most Recent Common Available Year ----
current_yr <- as.integer(format(Sys.Date(), "%Y"))
candidate_years <- seq(current_yr - 1L, 2020L, by = -1L)

check_year_available <- function(yr) {
  acs_ok <- tryCatch({
    res <- suppressMessages(get_acs(
      geography = "tract",
      variables = "B01001_001",
      state = "11",
      year = yr,
      geometry = FALSE
    ))
    nrow(res) > 0
  }, error = \(e) FALSE)
  if (!acs_ok) return(FALSE)
  
  tiger_ok <- tryCatch({
    res <- suppressMessages(school_districts(
      state = "11",
      type = "unified",
      year = yr,
      progress_bar = FALSE
    ))
    nrow(res) > 0
  }, error = \(e) FALSE)
  if (!tiger_ok) return(FALSE)
  
  ccd_ok <- tryCatch({
    res <- suppressMessages(get_education_data(
      level = "school-districts",
      source = "ccd",
      topic = "directory",
      filters = list(year = yr, fips = 11)
    ))
    nrow(res) > 0
  }, error = \(e) FALSE)
  
  isTRUE(acs_ok && tiger_ok && ccd_ok)
}

message("Probing candidate years (", max(candidate_years), " down to ", min(candidate_years), ") for all 3 sources...")
year_this <- NA_integer_
for (yr in candidate_years) {
  message("  Checking ", yr, "...")
  if (check_year_available(yr)) {
    year_this <- yr
    break
  }
}

if (is.na(year_this)) {
  stop("Could not find a common available year across ACS, TIGER/Line, and NCES CCD.")
}
message("Most recent common available year: ", year_this)

# 2. Check Cache Manifest & Skip Fetch if Up to Date ----
tracts_path    <- file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))
districts_path <- file.path(cache_dir, paste0("districts_geo_raw_", year_this, ".rds"))
nces_path      <- file.path(cache_dir, paste0("nces_ccd_raw_", year_this, ".rds"))
manifest_path  <- file.path(cache_dir, "latest_cached_year.txt")

cached_year <- if (file.exists(manifest_path)) {
  suppressWarnings(as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE))))
} else {
  NA_integer_
}

files_exist <- all(file.exists(c(tracts_path, districts_path, nces_path)))

if (!isTRUE(force_sync) && !is.na(cached_year) && cached_year == year_this && files_exist) {
  message("Cache is already up to date for year ", year_this, ". Skipping download.")
  quit(save = "no", status = 0)
}

# 3. Fetch & Cache All Three Sources for year_this ----
state_fips <- tidycensus::fips_codes |>
  filter(as.numeric(state_code) <= 56) |>
  pull(state_code) |>
  unique()

# 3a. Census Tract Population & Geometry (ACS 5-Year)
message("Fetching ACS tract populations and geometries for ", year_this, "...")
tract_pops_raw <- map(
  state_fips,
  \(fip) {
    get_acs(
      geography = "tract",
      variables = c(total_pop = "B01001_001"),
      state = fip,
      year = year_this,
      geometry = TRUE,
      output = "wide"
    )
  },
  .progress = "Downloading tracts"
) |>
  list_rbind() |>
  st_as_sf()

saveRDS(tract_pops_raw, tracts_path)

# 3b. TIGER/Line School District Boundaries (Unified, Secondary, Elementary)
message("Fetching TIGER/Line school district boundaries for ", year_this, "...")
districts_geo_raw <- map(
  state_fips,
  \(fip) {
    types <- c("unified", "secondary", "elementary")
    map(types, \(t) {
      tryCatch({
        df <- suppressMessages(
          school_districts(state = fip, type = t, year = year_this, progress_bar = FALSE)
        )
        if (is.null(df) || nrow(df) == 0) return(NULL)
        df |>
          select(GEOID, STATEFP, NAME, geometry) |>
          mutate(tiger_district_type = t)
      }, error = \(e) NULL)
    }) |>
      compact() |>
      bind_rows()
  },
  .progress = "Downloading school districts"
) |>
  compact() |>
  bind_rows() |>
  distinct(GEOID, .keep_all = TRUE) |>
  st_as_sf()

message("Fetched ", nrow(districts_geo_raw), " total TIGER/Line district boundaries across types: ",
        paste(names(table(districts_geo_raw$tiger_district_type)),
              table(districts_geo_raw$tiger_district_type), sep = "=", collapse = ", "))

saveRDS(districts_geo_raw, districts_path)

# 3c. NCES Common Core of Data (CCD) District Directory
message("Fetching NCES CCD directory data for ", year_this, "...")
nces_ccd_raw <- get_education_data(
  level = "school-districts",
  source = "ccd",
  topic = "directory",
  filters = list(year = year_this)
)

saveRDS(nces_ccd_raw, nces_path)

writeLines(as.character(year_this), manifest_path)
message("All raw datasets cached successfully for ", year_this, " in: ", cache_dir)