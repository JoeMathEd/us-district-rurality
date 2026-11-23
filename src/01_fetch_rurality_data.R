# 01_fetch_rurality_data.R
# Determine the most recent common year across ACS, TIGER/Line, and NCES CCD,
# fetch/cache annual datasets, cache 2020 Decennial Census block interior points,
# cache Census Urban Areas + USDA ERS 2020 RUCA tract codes (Extensions #3 & #4),
# and cache edfinr finance + ACS econ/C24030 + LEHD LODES8 datasets (Extensions #1 & #2).
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
library(edfinr)
library(lehdr)
library(furrr)

options(tigris_use_cache = TRUE)
options(tigris_cache_dir = cache_dir)
options(lehdr_use_cache = TRUE)
sf_use_s2(TRUE)

n_workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
if (!is.na(n_workers) && n_workers > 1L) {
  plan(multicore, workers = n_workers)
} else {
  plan(sequential)
}

state_fips <- tidycensus::fips_codes |>
  filter(as.numeric(state_code) <= 56) |>
  pull(state_code) |>
  unique()

fips_lookup <- tidycensus::fips_codes |>
  select(state_fip = state_code, state_abbr = state) |>
  distinct()

# 0A. Ensure 2020 Decennial Census Block Interior Points Are Cached ----
decennial_year <- 2020L
blocks_path    <- file.path(cache_dir, paste0("decennial_block_points_", decennial_year, ".rds"))

if (!file.exists(blocks_path)) {
  message("Fetching and caching ", decennial_year, " Decennial Census block points across ", length(state_fips), " jurisdictions...")
  decennial_block_points <- future_map(
    state_fips,
    \(fip) {
      suppressMessages(get_decennial(
        geography = "block",
        variables = c(blk_pop = "P1_001N", blk_hu = "H1_001N"),
        state = fip,
        year = decennial_year,
        geometry = TRUE,
        output = "wide"
      )) |>
        st_transform(5070) |>
        st_point_on_surface() |>
        mutate(
          state_fip   = fip,
          tract_geoid = stringr::str_sub(GEOID, 1, 11)
        ) |>
        select(block_geoid = GEOID, state_fip, tract_geoid, blk_pop, blk_hu)
    },
    .options = furrr_options(seed = TRUE)
  ) |>
    list_rbind() |>
    st_as_sf()
  
  saveRDS(decennial_block_points, blocks_path)
  message("Saved ", nrow(decennial_block_points), " decennial block interior points to: ", blocks_path)
  rm(decennial_block_points)
  gc()
} else {
  message("Decennial block cache already exists: ", blocks_path)
}

# 0B. Ensure USDA ERS 2020 Tract RUCA Codes Are Cached (Extension #3) ----
script_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root   <- if (length(script_flag) > 0) dirname(dirname(normalizePath(sub("^--file=", "", script_flag[1]), mustWork = FALSE))) else getwd()
local_ruca  <- file.path(repo_root, "data_raw", "RUCA-codes-2020-tract.csv")
ruca_path   <- file.path(cache_dir, "ruca_tracts_2020.rds")

if (!file.exists(ruca_path) || isTRUE(force_sync)) {
  if (!file.exists(local_ruca)) {
    stop("Missing ", local_ruca, ". Commit data_raw/RUCA-codes-2020-tract.csv to the repository first.")
  }
  message("Parsing and caching USDA ERS 2020 RUCA tract codes from: ", local_ruca)
  ruca_raw <- readr::read_csv(local_ruca, show_col_types = FALSE)
  
  geoid_col <- names(ruca_raw)[stringr::str_detect(tolower(names(ruca_raw)), "tract.*fips|fips.*tract|geoid")][1]
  prim_col  <- names(ruca_raw)[stringr::str_detect(tolower(names(ruca_raw)), "primary.*ruca|ruca1")][1]
  
  if (is.na(geoid_col) || is.na(prim_col)) {
    stop("Could not identify tract FIPS or primary RUCA columns in RUCA-codes-2020-tract.csv. Columns: ",
         paste(names(ruca_raw), collapse = ", "))
  }
  
  ruca_df <- ruca_raw |>
    transmute(
      tract_geoid  = stringr::str_pad(as.character(.data[[geoid_col]]), 11, pad = "0"),
      ruca_primary = as.integer(.data[[prim_col]])
    ) |>
    filter(!is.na(tract_geoid), !is.na(ruca_primary)) |>
    distinct(tract_geoid, .keep_all = TRUE)
  
  saveRDS(ruca_df, ruca_path)
  message("Saved ", nrow(ruca_df), " 2020 RUCA tract records to: ", ruca_path)
} else {
  message("USDA ERS 2020 RUCA cache already exists: ", ruca_path)
}

# 1. Probe Most Recent Common Available Year ----
current_yr <- as.integer(format(Sys.Date(), "%Y"))
candidate_years <- seq(current_yr - 1L, 2020L, by = -1L)

check_year_available <- function(yr) {
  acs_ok <- tryCatch({
    res <- suppressMessages(get_acs(
      geography = "tract",
      variables = c("B01001_001", "B09001_006", "B09001_010"),
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
      filters = list(year = yr, fips = c(11, 41))
    ))
    nrow(res) > 10
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

# 2. Check Cache Manifest & Required Datasets ----
tracts_path      <- file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))
districts_path   <- file.path(cache_dir, paste0("districts_geo_raw_", year_this, ".rds"))
nces_path        <- file.path(cache_dir, paste0("nces_ccd_raw_", year_this, ".rds"))
urban_areas_path <- file.path(cache_dir, paste0("urban_areas_raw_", year_this, ".rds"))
manifest_path    <- file.path(cache_dir, "latest_cached_year.txt")

if (!file.exists(urban_areas_path) || isTRUE(force_sync)) {
  message("Fetching and caching TIGER/Line Census Urban Areas for ", year_this, "...")
  urban_areas_raw <- suppressMessages(tigris::urban_areas(year = year_this, cb = FALSE, progress_bar = FALSE))
  saveRDS(urban_areas_raw, urban_areas_path)
  message("Saved ", nrow(urban_areas_raw), " Census Urban Areas to: ", urban_areas_path)
  rm(urban_areas_raw)
  gc()
}

# 2A. Extension #1: Cache National edfinr Finance Data (probes latest available fiscal year) ----
existing_fin <- sort(list.files(cache_dir, pattern = "^edfinr_finance_\\d{4}\\.rds$", full.names = TRUE), decreasing = TRUE)
if (length(existing_fin) == 0 || isTRUE(force_sync)) {
  message("Fetching national school district finance dataset via edfinr...")
  fin_df <- NULL
  fin_yr <- NA_integer_
  for (fy in seq(year_this, 2020L, by = -1L)) {
    res <- tryCatch(
      edfinr::get_finance_data(yr = fy, geo = "all", cpi_adj = "none"),
      error = \(e) NULL
    )
    if (!is.null(res) && nrow(res) > 100) {
      fin_df <- res
      fin_yr <- fy
      break
    }
  }
  if (is.null(fin_df)) {
    warning("Could not retrieve national edfinr finance dataset; Extension #1 columns will be NA.")
  } else {
    fin_out <- file.path(cache_dir, paste0("edfinr_finance_", fin_yr, ".rds"))
    saveRDS(fin_df, fin_out)
    message("Saved ", nrow(fin_df), " edfinr finance records (FY ", fin_yr, ") to: ", fin_out)
    rm(fin_df)
    gc()
  }
} else {
  message("edfinr finance cache already exists: ", existing_fin[1])
}

# 2B. Extension #2: Cache National LEHD LODES8 WAC & OD Summaries Across States ----
existing_wac <- sort(list.files(cache_dir, pattern = "^lodes8_wac_\\d{4}\\.rds$", full.names = TRUE), decreasing = TRUE)
existing_od  <- sort(list.files(cache_dir, pattern = "^lodes8_od_blocks_\\d{4}\\.rds$", full.names = TRUE), decreasing = TRUE)

if (length(existing_wac) == 0 || length(existing_od) == 0 || isTRUE(force_sync)) {
  lodes_fips <- state_fips[state_fips != "02"] # Alaska LODES coverage varies; extend if present
  lodes_st_tbl <- fips_lookup |>
    filter(state_fip %in% lodes_fips) |>
    mutate(st_abbr_lc = stringr::str_to_lower(state_abbr))
  
  lodes_yr <- NA_integer_
  for (yr in seq(year_this, 2021L, by = -1L)) {
    probe <- tryCatch(
      lehdr::grab_lodes(
        state = "id", year = yr, version = "LODES8", lodes_type = "wac",
        job_type = "JT01", segment = "S000", agg_geo = "block",
        download_dir = cache_dir, use_cache = TRUE
      ),
      error = \(e) NULL
    )
    if (!is.null(probe) && nrow(probe) > 0) {
      lodes_yr <- yr
      break
    }
  }
  
  if (!is.na(lodes_yr)) {
    message("Fetching LODES8 WAC and OD (main + aux) across ", nrow(lodes_st_tbl), " states for year ", lodes_yr, "...")
    lodes_per_state <- future_map(
      lodes_st_tbl$st_abbr_lc,
      \(st_lc) {
        wac_df <- tryCatch(
          suppressMessages(lehdr::grab_lodes(
            state = st_lc, year = lodes_yr, version = "LODES8", lodes_type = "wac",
            job_type = "JT01", segment = "S000", agg_geo = "block",
            download_dir = cache_dir, use_cache = TRUE
          )),
          error = \(e) NULL
        )
        
        wac_blk <- if (!is.null(wac_df) && nrow(wac_df) > 0) {
          wac_df |>
            transmute(
              block_geoid  = stringr::str_pad(as.character(w_geocode), 15, pad = "0"),
              work_jobs    = coalesce(as.numeric(C000), 0),
              work_ag_for  = coalesce(as.numeric(CNS01), 0),
              work_mining  = coalesce(as.numeric(CNS02), 0),
              work_mfg     = coalesce(as.numeric(CNS05), 0),
              work_ed_hlth = coalesce(as.numeric(CNS15), 0) + coalesce(as.numeric(CNS16), 0),
              work_ce01    = coalesce(as.numeric(CE01), 0),
              work_ce03    = coalesce(as.numeric(CE03), 0)
            )
        } else {
          NULL
        }
        
        od_main <- tryCatch(
          suppressMessages(lehdr::grab_lodes(
            state = st_lc, year = lodes_yr, version = "LODES8", lodes_type = "od",
            job_type = "JT01", segment = "S000", state_part = "main", agg_geo = "block",
            download_dir = cache_dir, use_cache = TRUE
          )),
          error = \(e) NULL
        )
        od_aux <- tryCatch(
          suppressMessages(lehdr::grab_lodes(
            state = st_lc, year = lodes_yr, version = "LODES8", lodes_type = "od",
            job_type = "JT01", segment = "S000", state_part = "aux", agg_geo = "block",
            download_dir = cache_dir, use_cache = TRUE
          )),
          error = \(e) NULL
        )
        
        od_both <- bind_rows(od_main, od_aux)
        od_pairs <- if (!is.null(od_both) && nrow(od_both) > 0) {
          od_both |>
            transmute(
              h_blk = stringr::str_pad(as.character(h_geocode), 15, pad = "0"),
              w_blk = stringr::str_pad(as.character(w_geocode), 15, pad = "0"),
              jobs  = coalesce(as.numeric(S000), 0)
            ) |>
            group_by(h_blk, w_blk) |>
            summarize(jobs = sum(jobs, na.rm = TRUE), .groups = "drop")
        } else {
          NULL
        }
        
        list(wac_blk = wac_blk, od_pairs = od_pairs)
      },
      .options = furrr_options(seed = TRUE)
    )
    
    lodes_wac_all <- map(lodes_per_state, "wac_blk") |> compact() |> bind_rows()
    lodes_od_all  <- map(lodes_per_state, "od_pairs") |> compact() |> bind_rows()
    
    wac_out <- file.path(cache_dir, paste0("lodes8_wac_", lodes_yr, ".rds"))
    od_out  <- file.path(cache_dir, paste0("lodes8_od_blocks_", lodes_yr, ".rds"))
    saveRDS(lodes_wac_all, wac_out)
    saveRDS(lodes_od_all, od_out)
    message("Saved LODES8 WAC (", nrow(lodes_wac_all), " blocks) and OD (", nrow(lodes_od_all), " block pairs) for ", lodes_yr)
    rm(lodes_per_state, lodes_wac_all, lodes_od_all)
    gc()
  } else {
    warning("Could not find available LODES8 year; Extension #2 LODES columns will be NA.")
  }
} else {
  message("LODES8 caches already exist: ", existing_wac[1], " and ", existing_od[1])
}

cached_year <- if (file.exists(manifest_path)) {
  suppressWarnings(as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE))))
} else {
  NA_integer_
}

files_exist <- all(file.exists(c(tracts_path, districts_path, nces_path, blocks_path, urban_areas_path, ruca_path)))

tracts_has_ext2_cols <- if (file.exists(tracts_path)) {
  all(c("age_5E", "med_hh_incE", "pov_belowE", "emp_totalE", "ag_min_mE") %in% names(readRDS(tracts_path)))
} else {
  FALSE
}

if (!isTRUE(force_sync) && !is.na(cached_year) && cached_year == year_this && files_exist && tracts_has_ext2_cols) {
  message("Cache is already up to date for year ", year_this, " (including B09001, ACS econ/C24030, decennial blocks, Urban Areas, and 2020 RUCA). Skipping annual ACS/TIGER/CCD download.")
  plan(sequential)
  quit(save = "no", status = 0)
}

# 3. Fetch & Cache All Three Annual Sources for year_this ----
message("Fetching ACS tract populations, socioeconomic & C24030 industry variables, and geometries for ", year_this, "...")
acs_vars <- c(
  total_pop  = "B01001_001",
  age_5      = "B09001_006",
  age_6_8    = "B09001_007",
  age_9_11   = "B09001_008",
  age_12_14  = "B09001_009",
  age_15_17  = "B09001_010",
  med_hh_inc = "B19013_001",
  hh_total   = "B19001_001",
  pov_univ   = "B17001_001",
  pov_below  = "B17001_002",
  lf_civ     = "B23025_003",
  lf_unemp   = "B23025_005",
  emp_total  = "C24030_001",
  ag_min_m   = "C24030_003",
  ag_min_f   = "C24030_030",
  const_m    = "C24030_006",
  const_f    = "C24030_033",
  mfg_m      = "C24030_007",
  mfg_f      = "C24030_034",
  retail_m   = "C24030_009",
  retail_f   = "C24030_036",
  ed_hlth_m  = "C24030_014",
  ed_hlth_f  = "C24030_041"
)

tract_pops_raw <- map(
  state_fips,
  \(fip) {
    get_acs(
      geography = "tract",
      variables = acs_vars,
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

saveRDS(districts_geo_raw, districts_path)

message("Fetching NCES CCD directory data for ", year_this, "...")
nces_ccd_raw <- tryCatch(
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(year = year_this)
  ),
  error = \(e) {
    message("Bulk national CCD query failed (", conditionMessage(e), "); fetching state-by-state by FIPS...")
    map(
      as.integer(state_fips),
      \(fip_int) {
        tryCatch(
          suppressMessages(get_education_data(
            level = "school-districts",
            source = "ccd",
            topic = "directory",
            filters = list(year = year_this, fips = fip_int)
          )),
          error = \(err) NULL
        )
      },
      .progress = "Downloading NCES CCD by state"
    ) |>
      compact() |>
      bind_rows()
  }
)

if (is.null(nces_ccd_raw) || nrow(nces_ccd_raw) == 0) {
  stop("Failed to retrieve NCES CCD directory records for year ", year_this, ".")
}

saveRDS(nces_ccd_raw, nces_path)

writeLines(as.character(year_this), manifest_path)
plan(sequential)
message("All raw datasets cached successfully for ", year_this, " in: ", cache_dir)