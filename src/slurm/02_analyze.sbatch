#!/bin/bash
#SBATCH --job-name=analyze_rurality
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:30:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/02_analyze_%j.out

module load R/4.3.2 gdal/3.8.1 udunits/2.2.28  # match your exact Borah module versions

CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"
OUT_DIR="/bsuscratch/$USER/district-rurality/outputs"
Rscript ~/us-district-rurality/src/02_analyze_and_plot_rurality.R "$CACHE_DIR" "$OUT_DIR"