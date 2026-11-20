#!/bin/bash
#SBATCH --job-name=fetch_rurality
#SBATCH --partition=bsudfq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=/bsuscratch/%u/district-rurality/logs/01_fetch_%j.out

module load R/4.3.2 gdal/3.8.1 udunits/2.2.28  # match your exact Borah module versions

CACHE_DIR="/bsuscratch/$USER/district-rurality/data_cache"
Rscript ~/us-district-rurality/src/01_fetch_rurality_data.R "$CACHE_DIR"