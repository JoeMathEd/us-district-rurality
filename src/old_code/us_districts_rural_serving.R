# R script to analyze US school districts by the estimated % of students who are rural
# Author: Joe Champion, Boise State University
# Created: Nov 2024, updated Oct 2026
# Last Run: 10/30/2026

args <- commandArgs(trailingOnly = TRUE)
cache_dir  <- if (length(args) >= 1) args[1] else "data_cache"
output_dir <- if (length(args) >= 2) args[2] else "outputs"

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# configuration ----
# R packages (run install.packages("<packagename>") if needed)
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(units)
library(educationdata)
library(googledrive)
library(googlesheets4)
library(viridis)
library(purrr)
library(scales)
library(ggrepel)


options(tigris_use_cache = TRUE)
options(tigris_cache_dir = cache_dir)

sf_use_s2(TRUE) # Ensure valid spherical geometry operations

# set the year to retrieve data (see ?get_acs for year options)
year_this <- 2023

# READ AND WRANGLE DATA ON CENSUS TRACTS ----
# List of state FIPS (excluding territories if restricting to state_fip < 60)
state_fips <- tidycensus::fips_codes |> 
  filter(as.numeric(state_code) <= 56) |> 
  pull(state_code) |> 
  unique()

tract_pops <- map(
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
  st_as_sf() |> 
  st_transform(5070) |> 
  st_make_valid() |> 
  mutate(
    tract_land_mi2 = set_units(st_area(geometry), "mi^2"),
    tract_pop_density = as.numeric(total_popE / tract_land_mi2),
    is_low_density = tract_pop_density < 500
  )

# READ AND WRANGLE DATA ON SCHOOL DISTRICTS ----

districts_geo <- map(
  state_fips,
  \(fip) {
    # Combine unified, secondary, and elementary to ensure complete spatial coverage
    types <- c("unified", "secondary", "elementary")
    
    map(types, \(t) {
      tryCatch(
        school_districts(state = fip, type = t, year = year_this, progress_bar = FALSE),
        error = \(e) NULL
      )
    }) |> 
      compact() |> 
      list_rbind()
  },
  .progress = "Downloading school districts"
) |> 
  list_rbind() |> 
  st_as_sf() |> 
  st_transform(5070) |> 
  st_make_valid() |> 
  select(GEOID, state_fip = STATEFP, district_name = NAME) |> 
  filter(as.numeric(state_fip) < 60) |> 
  filter(!st_is_empty(geometry)) |> 
  mutate(district_area_mi2 = set_units(st_area(geometry), "mi^2"))

# CALCULATE RURALITY BY INTERSECTION TRACTS WITH DISTRICTS ----
tract_pops_clean <- tract_pops |> 
  select(
    tract_geoid = GEOID, 
    total_pop = total_popE, 
    tract_pop_density, 
    is_low_density, 
    tract_land_mi2
  ) |> 
  mutate(state_fip = stringr::str_sub(tract_geoid, 1, 2))

districts_clean <- districts_geo |> 
  select(GEOID, state_fip, district_name, district_area_mi2)

intersections <- map(
  state_fips,
  \(fip) {
    tr_sub <- tract_pops_clean |> filter(state_fip == fip)
    dist_sub <- districts_clean |> filter(state_fip == fip)
    
    if (nrow(tr_sub) == 0 || nrow(dist_sub) == 0) return(NULL)
    
    st_intersection(tr_sub, dist_sub)
  },
  .progress = "Computing intersections by state"
) |> 
  compact() |> 
  list_rbind() |> 
  st_as_sf() |> 
  mutate(
    piece_area_mi2 = set_units(st_area(geometry), "mi^2"),
    area_prop = as.numeric(piece_area_mi2 / tract_land_mi2),
    # Apportion population proportionally by area slice
    est_piece_pop = total_pop * pmin(area_prop, 1.0)
  )

# SUMMARIZE DISTRICT-LEVEL RURAL SERVING METRICS ---------------------
districts_rural_summary <- intersections |> 
  st_drop_geometry() |> 
  group_by(GEOID, district_name) |> 
  summarize(
    n_intersecting_tracts = n(),
    est_total_pop = sum(est_piece_pop, na.rm = TRUE),
    est_rural_pop = sum(est_piece_pop[is_low_density], na.rm = TRUE),
    pct_pop_rural = ifelse(est_total_pop > 0, 100 * (est_rural_pop / est_total_pop), 0),
    weighted_mean_density = weighted.mean(tract_pop_density, est_piece_pop, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(
    districts_geo |> st_drop_geometry() |> select(GEOID, state_fip, district_area_mi2), 
    by = "GEOID"
  )


# APPEND NCES INFORMAITON ON DISTRICTS ----
# Pull enrollment, official NCES locale code, student-teacher ratio, etc
nces_ccd_raw <- get_education_data(
  level = "school-districts",
  source = "ccd",
  topic = "directory",
  filters = list(year = year_this) # Most recent CCD directory cycle in API
) 

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
    is_traditional_district = agency_type %in% c("1", "2")
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

# Merge with rural spatial summary
districts_complete <- districts_rural_summary |> 
  inner_join(nces_ccd_cleaned, by = "GEOID") |> 
  # Retain active, operational educational agencies that serve students
  filter(
    is_operational,
    is_traditional_district,
    enrollment > 0
  )

# EXPORT DATA FOR PUBLIC ACCESS ----

fips_lookup <- tidycensus::fips_codes |> 
  select(state_fip = state_code, state_abbr = state, state_name) |> 
  distinct()


districts_tabular <- districts_complete |> 
  st_drop_geometry() |> 
  left_join(fips_lookup, by = "state_fip") |> 
  relocate(GEOID, state_abbr, state_name, lea_name, .before = 1) |> 
  mutate(across(where(~ inherits(.x, "units")), as.numeric))

target_doc_name <- "US_School_Districts_Rural_Summary_2023"
target_sheet_name <- "Districts_Summary"

existing_file <- drive_find(
  pattern = paste0("^", target_doc_name, "$"),
  type = "spreadsheet",
  n_max = 1
)

if (nrow(existing_file) > 0) {
  # write to the existing file
  target_id <- existing_file$id[1]
  write_sheet(
    data = districts_tabular,
    ss = target_id,
    sheet = target_sheet_name
  )
  
  message("Overwrote tab '", target_sheet_name, "' in existing sheet: ", target_doc_name)
  
} else {
  # create the file
  ss <- gs4_create(
    name = target_doc_name,
    sheets = list(districts_tabular) |> setNames(target_sheet_name)
  )
  drive_share(
    file = as_id(ss),
    role = "reader",
    type = "anyone"
  )
  message("Created new document and granted public view access: ", gs4_browse(ss))
}

# VISUALIZE PERCENT RURAL


clean_district_name <- function(names) {
  # super elaborate name cleaning function (built using AI)
  names |> 
    str_replace_all(
      "(?i)\\s+in\\s+(the\\s+)?(count(y|ies)|cit(y|ies)).*$", 
      ""
    ) |> 
    str_replace_all(
      "(?i)\\s+and\\s+state(\\s+of.*)?$", 
      ""
    ) |> 
    
    str_to_title() |> 
    
    str_replace_all("(?i)\\b(No|Number)\\b\\.?\\s*:?\\s*\\d*", "") |> 
    str_replace_all("#\\s*\\d*", "") |> 
    
    str_replace_all("(?i)\\b(Re|Rj|Jt|R|C|J)[\\s\\-_/]*\\d+[a-z]?\\b", "") |> 
    str_replace_all("(?i)\\b\\d+[a-z]\\b", "") |> 
    str_replace_all("\\b\\d{1,4}\\b", "") |> 
    
    str_replace_all(
      "(?i)\\b(Public|Community|Consolidated|Local|Joint|Jt|Unified|Union|Elementary|High|Secondary|Exempted Village|Special|Independent|Charter)\\b", 
      ""
    ) |> 
    str_replace_all(
      "(?i)\\b(School\\s+District|District|Dist|Schools?|Sch\\s+Dist|SD|CUSD|USD|ISD|CSD|SSD)\\b", 
      ""
    ) |> 
    str_replace_all("(?i)\\b(County|Counties|City|Co)\\b", "") |> 
    
    str_replace_all("[\\.\\,\\(\\)\\[\\]\\-_/\\:\\;\\#]", " ") |> 
    
    str_replace_all("(?i)\\b(Re|Rj|Jt|No|J|R|C)\\b", "") |> 
    
    str_replace_all("(?i)\\b(In\\s+The\\s+Of|In\\s+The|Of\\s+The|In|Of|The|And|At|For)\\b", "") |> 
    
    str_replace_all("\\b(Ii|Iii|Iv|Vi|Vii|Viii)\\b", \(m) str_to_upper(m)) |> 
    
    str_squish() |> 
    str_remove("^[\\.\\,\\-\\s]+") |> 
    str_remove("[\\.\\,\\-\\s]+$")
}

fips_lookup <- tidycensus::fips_codes |> 
  select(state_fip = state_code, state_abbr = state, state_name) |> 
  distinct()

districts_complete_sf <- districts_complete |> 
  left_join(fips_lookup, by = "state_fip")

if (!inherits(districts_complete_sf, "sf")) {
  districts_complete_sf <- districts_complete_sf |> 
    left_join(districts_geo |> select(GEOID, geometry), by = "GEOID") |> 
    st_as_sf()
} else {
  districts_complete_sf <- st_as_sf(districts_complete_sf)
}

districts_complete_sf <- districts_complete_sf |> 
  mutate(
    short_name = clean_district_name(lea_name),
    short_name = if_else(short_name == "" | is.na(short_name), 
                         word(lea_name, 1), 
                         short_name)
  )


# SET UP FOLDERS FOR THE MAPS
folder_unlabeled <- file.path(output_dir, "state_district_maps", "unlabeled")
folder_labeled   <- file.path(output_dir, "state_district_maps", "labeled")

dir.create(folder_unlabeled, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_labeled, recursive = TRUE, showWarnings = FALSE)

states_to_plot <- fips_lookup |>
  filter(as.numeric(state_fip) <= 56) |>
  pull(state_name) |>
  unique()

walk(states_to_plot, \(st_name) {
  # DRAW THE MAPS
  st_data <- districts_complete_sf |> 
    filter(state_name == st_name)
  
  if (nrow(st_data) == 0 || all(st_is_empty(st_data$geometry))) return(NULL)
  
  n_districts <- nrow(st_data)
  
  # --- Dynamic Local Projection (Eliminates tilt) ---
  centroid_lon <- st_data |> 
    st_transform(4326) |> 
    st_union() |> 
    st_centroid() |> 
    st_coordinates() |> 
    _[, "X"]
  
  utm_zone <- floor((centroid_lon + 180) / 6) + 1
  state_epsg <- 32600 + utm_zone
  
  st_data_proj <- st_data |> 
    st_transform(state_epsg)
  
  # --- Canvas Sizing & Aspect Ratio ---
  bbox <- st_bbox(st_data_proj)
  dx <- bbox$xmax - bbox$xmin
  dy <- bbox$ymax - bbox$ymin
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
  
  # --- Base Choropleth (Shared) ---
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
  
  # --- 1. Export Unlabeled Map ---
  ggsave(
    filename = file.path(folder_unlabeled, paste0(clean_filename, "_rural_unlabeled.png")),
    plot = p_base,
    width = plot_w,
    height = plot_h,
    dpi = 300
  )
  
  # --- 2. Add Labels & Export Labeled Map ---
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
      family = "Arial Narrow",
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
    plot = p_labeled,
    width = plot_w,
    height = plot_h,
    dpi = 300
  )
  
}, .progress = "Generating unlabeled & labeled state maps")


# UPLOAD the maps to google folder ----
target_drive_url <- "https://drive.google.com/drive/folders/13e_XkfPcQ54zofzWJhbGnbw-Uc0lQL6V"

dest_parent <- as_id(target_drive_url)

local_folders <- c(
  "unlabeled" = folder_unlabeled,
  "labeled"   = folder_labeled
)

iwalk(local_folders, \(local_dir, subfolder_name) {
  
  if (!dir.exists(local_dir)) {
    warning("Directory does not exist: ", local_dir)
    return(NULL)
  }
  
  existing_remote_folder <- drive_ls(
    path = dest_parent,
    pattern = paste0("^", subfolder_name, "$"),
    type = "folder"
  )
  
  if (nrow(existing_remote_folder) == 0) {
    remote_folder <- drive_mkdir(
      name = subfolder_name,
      path = dest_parent
    )
  } else {
    remote_folder <- existing_remote_folder[1, ]
  }
  
  png_files <- list.files(local_dir, pattern = "\\.png$", full.names = TRUE)
  
  walk(
    png_files,
    \(file_path) {
      drive_upload(
        media = file_path,
        path = as_id(remote_folder),
        name = basename(file_path),
        overwrite = TRUE
      )
    },
    .progress = paste("Uploading", subfolder_name, "maps")
  )
})
