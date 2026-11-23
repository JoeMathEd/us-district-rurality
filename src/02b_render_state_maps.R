# 02b_render_state_maps.R
# Render 300 DPI unlabeled and labeled state choropleth maps from the cached
# spatial checkpoint districts_complete_sf_<year>.rds.
# Author: Joe Champion, Boise State University

args <- commandArgs(trailingOnly = TRUE)
cache_dir  <- if (length(args) >= 1) args[1] else "data_cache"
output_dir <- if (length(args) >= 2) args[2] else "outputs"
year_arg   <- if (length(args) >= 3 && args[3] != "" && toupper(args[3]) != "NA") as.integer(args[3]) else NA_integer_
web_dir    <- if (length(args) >= 4 && args[4] != "") args[4] else "docs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(sf)
library(tidycensus)
library(viridis)
library(scales)
library(ggrepel)
library(furrr)

sf_use_s2(FALSE)

# Cap plotting workers at 8 so parallel PNG raster buffers stay well under 14 GB physical RAM
n_workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
plot_workers <- min(n_workers, 8L)
if (!is.na(plot_workers) && plot_workers > 1L) {
  plan(multicore, workers = plot_workers)
} else {
  plan(sequential)
}
message("Configured 02b map rendering with ", plot_workers, " worker(s).")

manifest_path <- file.path(cache_dir, "latest_cached_year.txt")
year_this <- if (!is.na(year_arg)) {
  year_arg
} else if (file.exists(manifest_path)) {
  as.integer(trimws(readLines(manifest_path, n = 1, warn = FALSE)))
} else {
  stop("No year specified and ", manifest_path, " not found.")
}

spatial_checkpoint_path <- file.path(cache_dir, paste0("districts_complete_sf_", year_this, ".rds"))
if (!file.exists(spatial_checkpoint_path)) {
  stop("Missing spatial checkpoint: ", spatial_checkpoint_path, ". Run 02a_compute_spatial_metrics.R first.")
}

script_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir  <- if (length(script_flag) > 0) dirname(sub("^--file=", "", script_flag[1])) else "src"
source(file.path(script_dir, "utils", "district_name_helpers.R"))

districts_complete_sf <- readRDS(spatial_checkpoint_path) |>
  mutate(short_name = clean_district_name(lea_name, district_name))

fips_lookup <- tidycensus::fips_codes |>
  select(state_fip = state_code, state_abbr = state, state_name) |>
  distinct()

folder_unlabeled <- file.path(output_dir, "state_district_maps", "unlabeled")
folder_labeled   <- file.path(output_dir, "state_district_maps", "labeled")
dir.create(folder_unlabeled, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_labeled, recursive = TRUE, showWarnings = FALSE)

states_to_plot <- fips_lookup |>
  filter(as.numeric(state_fip) <= 56) |>
  pull(state_name) |>
  unique()

message("Rendering unlabeled & labeled 300 DPI state maps for ", year_this, "...")
future_walk(states_to_plot, \(st_name) {
  st_data <- districts_complete_sf |>
    filter(state_name == st_name) |>
    arrange(desc(district_area_mi2))

  if (nrow(st_data) == 0 || all(st_is_empty(st_data$geometry))) return(NULL)

  n_districts <- nrow(st_data)

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
      subtitle = "Block-dasymetric ages 5–17 share in low-density census tracts (< 500 people / sq mi)",
      caption = paste0("Source: NCES CCD, US Census ACS 5-Year & 2020 Decennial Blocks (", year_this, ")")
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

  ggsave(
    filename = file.path(folder_unlabeled, paste0(clean_filename, "_rural_unlabeled.png")),
    bg = "white",
    plot = p_base,
    width = plot_w,
    height = plot_h,
    dpi = 300
  )

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
message("Completed parallel map rendering for ", year_this, ".")