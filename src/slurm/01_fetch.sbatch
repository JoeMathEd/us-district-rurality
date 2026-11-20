#!/bin/bash
#SBATCH --job-name=fetch_rurality
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/01_fetch_%j.out

module load apptainer/1.2.5

SIF="/bsuscratch/$USER/containers/rocker_geospatial.sif"
CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"

apptainer exec --bind /bsuscratch/$USER --env R_LIBS=/bsuscratch/$USER/R_libs "$SIF" \
  Rscript ~/us-district-rurality/src/01_fetch_rurality_data.R "$CACHE_DIR"