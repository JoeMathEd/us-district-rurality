#!/bin/bash
#SBATCH --job-name=render_state_maps
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=24G
#SBATCH --time=00:20:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/02b_maps_%j.out

module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"
OUT_DIR="/bsuscratch/$USER/district-rurality/outputs"
WEB_DIR="$HOME/us-district-rurality/docs"
YEAR_ARG="${1:-NA}"

apptainer exec --bind /bsuscratch/$USER,$HOME --env R_LIBS=/bsuscratch/$USER/R_libs "$SIF" Rscript "$HOME/us-district-rurality/src/02b_render_state_maps.R" "$CACHE_DIR" "$OUT_DIR" "$YEAR_ARG" "$WEB_DIR"
