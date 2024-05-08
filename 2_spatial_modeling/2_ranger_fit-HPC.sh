#!/bin/bash

# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 2/13/2023

# NOTE: 
#	1) uses premade csv where each row = expanded combination of CV seed (10), response (6), and hour (24)

# - SLURM -
#SBATCH --job-name=RFvif3                # defined name for jobstats
#SBATCH --output='./logs/%a.out'
#SBATCH --partition=core                 # partition name
#SBATCH --time=00:30:00                  # walltime defined above 45min, 10min
#SBATCH --cpus-per-task=2
#SBATCH --mem=2GB                        # mem in GB
#SBATCH --array=[1-2900]

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
Rscript ./2_ranger_fit-HPC_by_fold.R $SLURM_ARRAY_TASK_ID

# - ENDING -
echo "Ended at:"
date
echo
end=`date +%s`
totTime=$((end-start))
echo Total time: $totTime sec