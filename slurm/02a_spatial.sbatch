#!/bin/bash
#SBATCH --job-name=spatial_rurality
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=36G
#SBATCH --time=00:25:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/02a_spatial_%j.out

module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"
OUT_DIR="/bsuscratch/$USER/district-rurality/outputs"
YEAR_ARG="${1:-NA}"

apptainer exec --bind /bsuscratch/$USER,$HOME --env R_LIBS=/bsuscratch/$USER/R_libs "$SIF" Rscript "$HOME/us-district-rurality/src/02a_compute_spatial_metrics.R" "$CACHE_DIR" "$OUT_DIR" "$YEAR_ARG"
