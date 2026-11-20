#!/bin/bash
#SBATCH --job-name=analyze_rurality
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:30:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/02_analyze_%j.out

module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"
OUT_DIR="/bsuscratch/$USER/district-rurality/outputs"

apptainer exec --bind /bsuscratch/$USER --env R_LIBS=/bsuscratch/$USER/R_libs "$SIF" \
  Rscript ~/us-district-rurality/src/02_analyze_and_plot_rurality.R "$CACHE_DIR" "$OUT_DIR"