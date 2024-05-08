#!/bin/bash

# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 2/13/2023

# NOTE: 

# - SLURM -
#SBATCH --job-name=SURFS                 # defined name for jobstats
#SBATCH --output='./3_post_modeling/logs/%a-avgs.out'
#SBATCH --partition=core                 # partition name
#SBATCH --time=00:05:00                  # walltime
#SBATCH --cpus-per-task=2
#SBATCH --mem=2GB                        # mem in GB
#SBATCH --array=[3-29] # 29

date_time=`date +%Y%m%d_%H%M%S`
echo "The starting date_time: " $date_time
echo
echo "SLURM_JOBID: "$SLURM_JOBID
echo "SLURM_ARRAY_JOB_ID: "$SLURM_ARRAY_JOB_ID
echo "SLURM ARRAY TASK ID: "$SLURM_ARRAY_TASK_ID
echo

# - MODULES -
module load anaconda3
conda activate geospatial-cq

echo
echo "---------Entering Rscript---------"
# - R SCRIPT -
Rscript ./3_post_modeling/2_prediction_surface_avg-HPC.R $SLURM_ARRAY_TASK_ID

# - ENDING -
echo "Ended at:"
date
echo
