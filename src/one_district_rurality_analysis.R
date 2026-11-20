# ==============================================================================
# Standalone Single-District Rural Student Analysis
# ==============================================================================

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(units)
library(educationdata)
library(patchwork)
library(scales)

options(tigris_use_cache = TRUE)
sf_use_s2(TRUE)

year_this <- 2025

# ------------------------------------------------------------------------------
# 1. Pipeline Function: Analyze Any District Geometry
# ------------------------------------------------------------------------------

# custom function to analyze one district
analyze_single_district <- function(district_sf, 
                                    district_label = "Target District",
                                    leaid = NULL,
                                    year = 2023) {
  
  # Step A: Enforce standard projection and valid geometry
  dist_proj <- district_sf |> 
    st_transform(5070) |> 
    st_make_valid() |> 
    st_union() |> 
    st_as_sf() |> 
    rename(geometry = x)
  
  dist_area_mi2 <- set_units(st_area(dist_proj), "mi^2")
  
  # Step B: Identify overlapping State FIPS to pull minimal tracts
  dist_wgs84 <- st_transform(dist_proj, 4326)
  
  states_geo <- states(year = year, cb = TRUE, progress_bar = FALSE) |> 
    st_transform(4326)
  
  overlapping_fips <- states_geo[st_intersects(states_geo, dist_wgs84, sparse = FALSE), ]$STATEFP
  
  if (length(overlapping_fips) == 0) {
    stop("Could not determine overlapping state FIPS for this boundary.")
  }
  
  # Step C: Retrieve Tracts for overlapping state(s)
  tract_pops <- map(
    overlapping_fips,
    \(fip) {
      get_acs(
        geography = "tract",
        variables = c(total_pop = "B01001_001"),
        state = fip,
        year = year,
        geometry = TRUE,
        output = "wide"
      )
    }
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
  
  # Filter tracts that spatially touch the district boundary
  touching_indices <- st_intersects(tract_pops, dist_proj, sparse = FALSE)
  tract_sub <- tract_pops[touching_indices, ]
  
  if (nrow(tract_sub) == 0) {
    stop("No census tracts intersected the district boundary.")
  }
  
  # Step D: Area-weighted polygon intersection
  pieces <- st_intersection(tract_sub, dist_proj) |> 
    mutate(
      piece_area_mi2 = set_units(st_area(geometry), "mi^2"),
      area_prop = as.numeric(piece_area_mi2 / tract_land_mi2),
      est_piece_pop = total_popE * pmin(area_prop, 1.0)
    )
  
  # Step E: Compute rurality statistics
  est_total_pop <- sum(pieces$est_piece_pop, na.rm = TRUE)
  est_rural_pop <- sum(pieces$est_piece_pop[pieces$is_low_density], na.rm = TRUE)
  pct_rural     <- ifelse(est_total_pop > 0, 100 * (est_rural_pop / est_total_pop), 0)
  mean_density  <- weighted.mean(pieces$tract_pop_density, pieces$est_piece_pop, na.rm = TRUE)
  
  # Step F: Query NCES CCD if LEAID is present
  nces_stats <- tibble(
    enrollment = NA_real_,
    teachers_fte = NA_real_,
    n_schools = NA_real_,
    locale = "Not Supplied / Custom Shapefile"
  )
  
  if (!is.null(leaid)) {
    clean_id <- stringr::str_pad(as.character(leaid), 7, pad = "0")
    try({
      raw_ccd <- get_education_data(
        level = "school-districts",
        source = "ccd",
        topic = "directory",
        filters = list(year = year, leaid = clean_id)
      )
      if (nrow(raw_ccd) > 0) {
        nces_stats <- tibble(
          enrollment   = raw_ccd$enrollment[1],
          teachers_fte = raw_ccd$teachers_total_fte[1],
          n_schools    = raw_ccd$number_of_schools[1],
          locale       = as.character(raw_ccd$urban_centric_locale[1])
        )
      }
    }, silent = TRUE)
  }
  
  # Step G: Build Summary Table
  summary_df <- tibble(
    Metric = c(
      "Rural Population Share",
      "Est. Total Population",
      "Est. Rural Population",
      "Weighted Pop Density",
      "Intersecting Tracts",
      "NCES Enrollment",
      "NCES Locale Code"
    ),
    Value = c(
      paste0(round(pct_rural, 1), "%"),
      scales::comma(round(est_total_pop)),
      scales::comma(round(est_rural_pop)),
      paste0(round(mean_density, 1), " / mi²"),
      as.character(nrow(pieces)),
      ifelse(is.na(nces_stats$enrollment), "N/A", scales::comma(nces_stats$enrollment)),
      nces_stats$locale
    )
  )
  
  # Step H: Dynamic Local UTM for un-tilted cartography
  centroid_lon <- dist_wgs84 |> 
    st_centroid() |> 
    st_coordinates() |> 
    _[, "X"]
  utm_zone <- floor((centroid_lon + 180) / 6) + 1
  local_crs <- 32600 + utm_zone
  
  pieces_local <- st_transform(pieces, local_crs)
  dist_local   <- st_transform(dist_proj, local_crs)
  
  # Step I: Visualizations
  # Left: Spatial tract layout within district
  p_map <- ggplot() +
    geom_sf(data = pieces_local, aes(fill = is_low_density), color = "white", linewidth = 0.3) +
    geom_sf(data = dist_local, fill = NA, color = "#0f172a", linewidth = 0.8) +
    scale_fill_manual(
      values = c("TRUE" = "#8fb996", "FALSE" = "#1e293b"),
      labels = c("TRUE" = "Rural (<500/mi²)", "FALSE" = "Urban/Suburban (≥500/mi²)"),
      name = "Tract Density"
    ) +
    labs(
      title = district_label,
      subtitle = paste0("Total Area: ", round(as.numeric(dist_area_mi2), 1), " sq mi")
    ) +
    theme_void(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey30", size = 10),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Right: Monospace Metric Card
  card_text <- paste0(
    "SUMMARY METRICS\n",
    "────────────────────────────────────────\n",
    "• Estimated Rural:       ", round(pct_rural, 1), "%\n",
    "• Total Population:      ", scales::comma(round(est_total_pop)), "\n",
    "• Rural Population:      ", scales::comma(round(est_rural_pop)), "\n",
    "• Weighted Mean Density: ", round(mean_density, 1), " / mi²\n",
    "• Intersecting Tracts:   ", nrow(pieces), "\n\n",
    "NCES ADMINISTRATIVE DATA\n",
    "────────────────────────────────────────\n",
    "• Student Enrollment:    ", ifelse(is.na(nces_stats$enrollment), "N/A", scales::comma(nces_stats$enrollment)), "\n",
    "• FTE Teachers:          ", ifelse(is.na(nces_stats$teachers_fte), "N/A", round(nces_stats$teachers_fte, 1)), "\n",
    "• School Facilities:     ", ifelse(is.na(nces_stats$n_schools), "N/A", nces_stats$n_schools), "\n",
    "• Urban-Centric Locale:  ", nces_stats$locale
  )
  
  p_card <- ggplot() +
    annotate(
      "text", x = 0.05, y = 0.5, 
      label = card_text, 
      family = "mono", size = 3.6, hjust = 0, vjust = 0.5
    ) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
  
  combined_dashboard <- p_map + p_card + plot_layout(widths = c(1.3, 1))
  
  return(list(
    metrics = summary_df,
    summary_vector = c(pct_rural = pct_rural, est_pop = est_total_pop, density = mean_density),
    pieces = pieces,
    dashboard = combined_dashboard
  ))
}

# ------------------------------------------------------------------------------
# 2. Execution Options
# ------------------------------------------------------------------------------

### OPTION A: User defined Custom Shapefile / GeoJSON
# shapefile_path <- "path/to/my_district.shp" # or .geojson
# user_district <- st_read(shapefile_path)
# result <- analyze_single_district(user_district, district_label = "Custom District Boundary")
# print(result$dashboard)


### OPTION B: Automated Download by State & District Name (tigris)
get_and_analyze_district <- function(state, name_pattern, year = 2023) {
  # Search unified districts first, then secondary/elementary if missing
  types <- c("unified", "secondary", "elementary")
  d_geo <- NULL
  
  for (tp in types) {
    try({
      found <- school_districts(state = state, type = tp, year = year, progress_bar = FALSE) |> 
        filter(str_detect(NAME, regex(name_pattern, ignore_case = TRUE)))
      if (nrow(found) > 0) {
        d_geo <- found[1, ]
        break
      }
    }, silent = TRUE)
  }
  
  if (is.null(d_geo)) stop(paste("No district matching pattern", name_pattern, "found in", state))
  
  analyze_single_district(
    district_sf = d_geo,
    district_label = d_geo$NAME,
    leaid = d_geo$GEOID,
    year = year
  )
}

# Example #1 - Boise, ID
res <- get_and_analyze_district(state = "ID", name_pattern = "Boise")
print(res$dashboard)
ggsave("rural/plots/single_districts/boise_district_analysis.png", plot = res$dashboard, width = 10, height = 5.5, dpi = 300)

res <- get_and_analyze_district(state = "ID", name_pattern = "Nampa")
print(res$dashboard)
ggsave("rural/plots/single_districts/nampa_district_analysis.png", plot = res$dashboard, width = 10, height = 5.5, dpi = 300)

res <- get_and_analyze_district(state = "ID", name_pattern = "Blaine")
print(res$dashboard)
ggsave("rural/plots/single_districts/blaine_district_analysis.png", plot = res$dashboard, width = 10, height = 5.5, dpi = 300)


# Example #2 - Helix Charter High School, La Mesa, CA
# get official Census boundaries for the City of La Mesa, CA
helix_sf <- places(state = "CA", cb = TRUE, year = year_this, progress_bar = FALSE) |> 
  filter(NAME == "La Mesa") |> 
  select(GEOID, NAME, geometry)

res_helix <- analyze_single_district(
  district_sf = helix_sf,
  district_label = "Helix Charter High School (La Mesa Attendance Zone)",
  leaid = "0616230",   # Parent LEAID: Grossmont Union High
  year = min(year_this, 2023) # most recent is 2023
)
print(res_helix$dashboard)
ggsave(
  filename = "rural/plots/single_districts/helix_charter_rural_analysis.png",
  plot = res_helix$dashboard,
  width = 11,
  height = 6.5,
  dpi = 300
)
