#!/bin/bash

#SBATCH --chdir=/work/gauzens
#SBATCH --job-name=the_simple_one
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0-00:30:00
#SBATCH --cpus-per-task=1
#SBATCH --output=/work/%u/%x-%A-%a.out

# load R software
module load foss/2019b R/4.0.0-2 


line=$(sed -n "$SLURM_ARRAY_TASK_ID p" $1)
Rscript --vanilla /home/gauzens/allometric_preferences_model/The_R_script.R $line
