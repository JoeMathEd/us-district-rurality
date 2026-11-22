#!/bin/bash
#SBATCH --job-name=web_data_rurality
#SBATCH --partition=bsudfq
#SBATCH --cpus-per-task=2
#SBATCH --mem=8G
#SBATCH --time=00:15:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/03_web_data_%j.out

source /etc/profile
module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
REPO_DIR="$HOME/us-district-rurality"
WORK_DIR="/bsuscratch/$USER/district-rurality"
CACHE_DIR="$WORK_DIR/data_cache"
WEB_DATA_DIR="$REPO_DIR/docs/data"
OVERRIDE_DIR="$REPO_DIR/data_raw/state_assessments"

# 1. Export simplified WGS84 GeoJSON from cached spatial checkpoint
apptainer exec --bind "/bsuscratch/$USER,$HOME" --env R_LIBS="/bsuscratch/$USER/R_libs" "$SIF" Rscript "$REPO_DIR/src/03_export_web_poc.R" "$CACHE_DIR" "$WEB_DATA_DIR"

# 2. Fetch/update Grade 3, 5, 8 math proficiency JSON
apptainer exec --bind "/bsuscratch/$USER,$HOME" --env R_LIBS="/bsuscratch/$USER/R_libs" "$SIF" Rscript "$REPO_DIR/src/04_fetch_and_clean_assessments.R" "$CACHE_DIR" "$WEB_DATA_DIR" "$OVERRIDE_DIR"