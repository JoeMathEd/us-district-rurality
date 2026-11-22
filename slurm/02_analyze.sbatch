#!/bin/bash
#SBATCH --job-name=analyze_rurality
#SBATCH --partition=bsudfq
#SBATCH --cpus-per-task=16
#SBATCH --mem=32G
#SBATCH --time=00:30:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/02_analyze_%j.out

source /etc/profile
module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
REPO_DIR="$HOME/us-district-rurality"
WORK_DIR="/bsuscratch/$USER/district-rurality"
CACHE_DIR="$WORK_DIR/data_cache"
OUT_DIR="$WORK_DIR/outputs"
WEB_DIR="$WORK_DIR/docs"

YEAR_ARG="${1:-NA}"
RECOMPUTE_ARG="${2:-FALSE}"

# 1. Run spatial analysis, tabular summary, and static PNG maps (outputs + web docs/maps)
apptainer exec --bind "/bsuscratch/$USER,$HOME" --env R_LIBS="/bsuscratch/$USER/R_libs" "$SIF" Rscript "$REPO_DIR/src/02_analyze_and_plot_rurality.R" "$CACHE_DIR" "$OUT_DIR" "$YEAR_ARG" "$RECOMPUTE_ARG" "$WEB_DIR"

# 2. Export simplified WGS84 GeoJSON into docs/data/
apptainer exec --bind "/bsuscratch/$USER,$HOME" --env R_LIBS="/bsuscratch/$USER/R_libs" "$SIF" Rscript "$REPO_DIR/src/03_export_web_poc.R" "$CACHE_DIR" "$WEB_DIR/data"