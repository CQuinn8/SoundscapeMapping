#!/bin/bash

# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 3/7/2023

# - SLURM -
#SBATCH --job-name=PDPs                  # defined name for jobstats
#SBATCH --output='./3_post_modeling/logs/%a-pdp.out'
#SBATCH --partition=core                 # partition name
#SBATCH --time=02:00:00                  # walltime defined 
#SBATCH --cpus-per-task=2
#SBATCH --mem=1GB                        # mem in GB
#SBATCH --array=9,24 # 29

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
Rscript ./3_post_modeling/6_PDP_calculations-HPC.R $SLURM_ARRAY_TASK_ID

# - ENDING -
echo "Ended at:"
date
echo
