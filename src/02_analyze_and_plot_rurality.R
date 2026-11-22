# 02_analyze_and_plot_rurality.R
# Compute tract-to-district spatial intersections, merge NCES CCD metrics, and render state maps
# Uses parallel processing (furrr) and caches the spatial join checkpoint to speed up plot iterations
# Author: Joe Champion, Boise State University

args <- commandArgs(trailingOnly = TRUE)
cache_dir         <- if (length(args) >= 1) args[1] else "data_cache"
output_dir        <- if (length(args) >= 2) args[2] else "outputs"
year_arg          <- if (length(args) >= 3 && args[3] != "" && toupper(args[3]) != "NA") as.integer(args[3]) else NA_integer_
recompute_spatial <- if (length(args) >= 4) as.logical(args[4]) else FALSE
web_dir           <- if (length(args) >= 5 && args[5] != "") args[5] else "docs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Configuration ----
library(tidyverse)
library(sf)
library(tidycensus)
library(units)
library(viridis)
library(scales)
library(ggrepel)
library(furrr)

sf_use_s2(TRUE)

# Configure parallel workers from Slurm CPU allocation ----
n_workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
if (!is.na(n_workers) && n_workers > 1L) {
  plan(multicore, workers = n_workers)
} else {
  plan(sequential)
}
message("Configured furrr with ", n_workers, " worker(s).")

# Resolve target year from CLI argument or latest_cached_year.txt manifest ----
manifest_path <- file.path(cache_dir, "latest_cached_year.txt")
year_this <- if (!is.na(year_arg)) {
  year_arg
} else if (file.exists(manifest_path)) {
  as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE)))
} else {
  stop("No year specified and ", manifest_path, " not found. Run 01_fetch_rurality_data.R first.")
}

message("Running rurality analysis and mapping for year: ", year_this)

# Checkpoint path for joined spatial + NCES dataset ----
spatial_checkpoint_path <- file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))

fips_lookup <- tidycensus::fips_codes |>
  select(state_fip = state_code, state_abbr = state, state_name) |>
  distinct()

if (!isTRUE(recompute_spatial) && file.exists(spatial_checkpoint_path)) {
  message("Loading cached spatial checkpoint: ", spatial_checkpoint_path)
  districts_complete_sf <- readRDS(spatial_checkpoint_path)
} else {
  # 1. Load Cached Raw Datasets ----
  tracts_path    <- file.path(cache_dir, paste0("tract_pops_raw_", year_this, ".rds"))
  districts_path <- file.path(cache_dir, paste0("districts_geo_raw_", year_this, ".rds"))
  nces_path      <- file.path(cache_dir, paste0("nces_ccd_raw_", year_this, ".rds"))
  
  if (!all(file.exists(c(tracts_path, districts_path, nces_path)))) {
    stop("Missing cached .rds files for ", year_this, " in ", cache_dir, ". Run 01_fetch_rurality_data.R first.")
  }
  
  tract_pops_raw    <- readRDS(tracts_path)
  districts_geo_raw <- readRDS(districts_path)
  nces_ccd_raw      <- readRDS(nces_path)
  
  # 2. Project & Wrangle Census Tracts (Equal-Area Albers EPSG:5070) ----
  tract_pops_clean <- tract_pops_raw |>
    st_transform(5070) |>
    st_make_valid() |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      total_pop         = coalesce(total_popE, 0),
      tract_land_mi2    = set_units(st_area(geometry), "mi^2"),
      tract_pop_density = if_else(
        as.numeric(tract_land_mi2) > 0,
        as.numeric(total_pop / tract_land_mi2),
        0
      ),
      is_low_density    = tract_pop_density < 500,
      state_fip         = stringr::str_sub(GEOID, 1, 2)
    ) |>
    select(
      tract_geoid = GEOID,
      state_fip,
      total_pop,
      tract_pop_density,
      is_low_density,
      tract_land_mi2
    )
  
  # 3. Project & Wrangle School District Boundaries ----
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
  
  districts_clean <- districts_geo |>
    select(GEOID, state_fip, district_name, district_area_mi2)
  
  # 4. Calculate Rurality by Intersecting Tracts with Districts (Parallelized) ----
  message("Computing state-by-state tract-to-district intersections...")
  state_fips <- unique(districts_clean$state_fip)
  
  intersections <- future_map(
    state_fips,
    \(fip) {
      tr_sub   <- tract_pops_clean |> filter(state_fip == fip)
      dist_sub <- districts_clean |> filter(state_fip == fip)
      
      if (nrow(tr_sub) == 0 || nrow(dist_sub) == 0) return(NULL)
      
      suppressWarnings(st_intersection(tr_sub, dist_sub))
    },
    .options = furrr_options(seed = TRUE)
  ) |>
    compact() |>
    bind_rows() |>
    st_as_sf() |>
    mutate(
      piece_area_mi2 = set_units(st_area(geometry), "mi^2"),
      area_prop      = if_else(
        as.numeric(tract_land_mi2) > 0,
        as.numeric(piece_area_mi2 / tract_land_mi2),
        0
      ),
      est_piece_pop  = total_pop * pmin(area_prop, 1.0)
    )
  
  # 5. Summarize District-Level Rural Serving Metrics ----
  districts_rural_summary <- intersections |>
    st_drop_geometry() |>
    group_by(GEOID, district_name) |>
    summarize(
      n_intersecting_tracts = n(),
      est_total_pop         = sum(est_piece_pop, na.rm = TRUE),
      est_rural_pop         = sum(est_piece_pop[is_low_density], na.rm = TRUE),
      pct_pop_rural         = if_else(est_total_pop > 0, 100 * (est_rural_pop / est_total_pop), NA_real_),
      weighted_mean_density = if_else(
        est_total_pop > 0,
        weighted.mean(tract_pop_density, est_piece_pop, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    right_join(
      districts_geo |> st_drop_geometry() |> select(GEOID, state_fip, district_area_mi2, tiger_district_type),
      by = "GEOID"
    )
  
  # 6. Clean & Merge NCES CCD Directory Data ----
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
      district_name = coalesce(district_name, lea_name),
      lea_name      = coalesce(lea_name, district_name),
      pct_pop_rural = if_else(is.na(enrollment) | enrollment <= 0, NA_real_, pct_pop_rural)
    )
  
  # Attach state metadata and district geometries, sorting largest area first so elementary
  # districts render on top of overlapping secondary/union high districts
  districts_complete_sf <- districts_complete |>
    left_join(fips_lookup, by = "state_fip") |>
    left_join(districts_geo |> select(GEOID, geometry), by = "GEOID") |>
    arrange(desc(district_area_mi2)) |>
    st_as_sf()
  
  saveRDS(districts_complete_sf, spatial_checkpoint_path)
  message("Saved spatial checkpoint to: ", spatial_checkpoint_path)
  
  # 7. Export Tabular Summary ----
  districts_tabular <- districts_complete_sf |>
    st_drop_geometry() |>
    relocate(GEOID, state_abbr, state_name, lea_name, .before = 1) |>
    mutate(across(where(~ inherits(.x, "units")), as.numeric))
  
  csv_out_path <- file.path(output_dir, paste0("US_School_Districts_Rural_Summary_", year_this, ".csv"))
  rds_out_path <- file.path(output_dir, paste0("US_School_Districts_Rural_Summary_", year_this, ".rds"))
  
  write_csv(districts_tabular, csv_out_path)
  saveRDS(districts_tabular, rds_out_path)
  message("Saved tabular summaries to: ", csv_out_path, " and ", rds_out_path)
}

# 8. Visualize Percent Rural by State (Parallelized) ----
# 8. Visualize Percent Rural by State (Parallelized) ----
source(file.path(dirname(sys.frame(1)$ofile %||% "src/02_analyze_and_plot_rurality.R"), "utils", "district_name_helpers.R"))

districts_complete_sf <- districts_complete_sf |>
  mutate( short_name = clean_district_name(lea_name, district_name) )

folder_unlabeled <- file.path(output_dir, "state_district_maps", "unlabeled")
folder_labeled   <- file.path(output_dir, "state_district_maps", "labeled")

dir.create(folder_unlabeled, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_labeled, recursive = TRUE, showWarnings = FALSE)

states_to_plot <- fips_lookup |>
  filter(as.numeric(state_fip) <= 56) |>
  pull(state_name) |>
  unique()

message("Rendering unlabeled & labeled state maps in parallel...")
future_walk(states_to_plot, \(st_name) {
  st_data <- districts_complete_sf |>
    filter(state_name == st_name) |>
    arrange(desc(district_area_mi2))
  
  if (nrow(st_data) == 0 || all(st_is_empty(st_data$geometry))) return(NULL)
  
  n_districts <- nrow(st_data)
  
  # Dynamic Local Projection (UTM zone based on state centroid)
  centroid_lon <- st_data |>
    st_transform(4326) |>
    st_union() |>
    st_centroid() |>
    st_coordinates() |>
    _[, "X"]
  
  utm_zone   <- floor((centroid_lon + 180) / 6) + 1
  state_epsg <- 32600 + utm_zone
  
  st_data_proj <- st_data |>
    st_transform(state_epsg)
  
  # Canvas Sizing & Aspect Ratio
  bbox         <- st_bbox(st_data_proj)
  dx           <- bbox$xmax - bbox$xmin
  dy           <- bbox$ymax - bbox$ymin
  aspect_ratio <- as.numeric(dx / dy)
  
  base_dim <- case_when(
    n_districts < 50   ~ 9,
    n_districts < 150  ~ 11,
    n_districts < 400  ~ 14,
    n_districts < 750  ~ 17,
    TRUE               ~ 21
  )
  
  if (aspect_ratio >= 1) {
    plot_w <- base_dim
    plot_h <- round(base_dim / aspect_ratio, 2)
  } else {
    plot_h <- base_dim
    plot_w <- round(base_dim * aspect_ratio, 2)
  }
  
  plot_w <- max(plot_w, 8)
  plot_h <- max(plot_h, 7)
  
  # Base Choropleth
  p_base <- ggplot(st_data_proj) +
    geom_sf(aes(fill = pct_pop_rural), color = "grey35", linewidth = 0.2) +
    scale_fill_gradient(
      low = "#1e293b",
      high = "#8fb996",
      na.value = "#ccc",
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      name = "% Rural\nStudents",
      labels = \(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Estimated Percentage of Rural Students:", st_name),
      subtitle = "Rural-centric definition: census tract population density < 500 people / sq mi",
      caption = paste0("Source: NCES CCD & US Census ACS 5-Year Estimates, ", year_this)
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey30", size = 9, margin = margin(b = 10)),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  clean_filename <- st_name |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_")
  
  # 1. Export Unlabeled Map
  ggsave(
    filename = file.path(folder_unlabeled, paste0(clean_filename, "_rural_unlabeled.png")),
    bg = "white",
    plot = p_base,
    width = plot_w,
    height = plot_h,
    dpi = 300
  )
  
  # 2. Add Labels & Export Labeled Map
  repel_overlaps <- case_when(
    n_districts > 500 ~ 200,
    n_districts > 200 ~ 100,
    TRUE              ~ 50
  )
  
  st_points <- suppressWarnings(st_point_on_surface(st_data_proj)) |>
    mutate(
      x = st_coordinates(geometry)[, 1],
      y = st_coordinates(geometry)[, 2],
      area_num = as.numeric(district_area_mi2),
      area_tertile = ntile(area_num, 3),
      label_size = if_else(area_tertile == 1, 1.9, 2.6)
    ) |>
    st_drop_geometry()
  
  p_labeled <- p_base +
    geom_text_repel(
      data = st_points,
      aes(x = x, y = y, label = short_name, size = label_size),
      color = "#0f172a",
      family = "sans",
      fontface = "bold",
      bg.color = alpha("white", 0.75),
      bg.r = 0.06,
      segment.color = "grey45",
      segment.size = 0.2,
      min.segment.length = 0.25,
      box.padding = 0.12,
      point.padding = 0.08,
      max.overlaps = repel_overlaps,
      show.legend = FALSE
    ) +
    scale_size_identity()
  
  ggsave(
    filename = file.path(folder_labeled, paste0(clean_filename, "_rural_labeled.png")),
    bg = "white",
    plot = p_labeled,
    width = plot_w,
    height = plot_h,
    dpi = 300
  )
  
  if (!is.na(web_dir)) {
    web_labeled_dir <- file.path(web_dir, "maps", "labeled")
    dir.create(web_labeled_dir, recursive = TRUE, showWarnings = FALSE)
    st_abbr <- tolower(st_data$state_abbr[1])
    ggsave(
      filename = file.path(web_labeled_dir, paste0(st_abbr, ".png")),
      bg = "white",
      plot = p_labeled,
      width = plot_w,
      height = plot_h,
      dpi = 300
    )
  }
}, .options = furrr_options(seed = TRUE))

plan(sequential)
message("Completed parallel rendering for ", year_this, ".")