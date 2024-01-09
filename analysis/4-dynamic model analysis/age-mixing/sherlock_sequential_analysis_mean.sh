#!/bin/sh
#                     # lines starting with #SBATCH is an instruction to the job scheduler
#SBATCH --job-name=hailey   # Job name
#SBATCH --mail-type=ALL         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=haileyjp@stanford.edu # Where to send mail  
#SBATCH --nodes=1                   # Use one node
#SBATCH --ntasks=1                  # Run a single task
#SBATCH --mem-per-cpu=128gb           # Memory per processor
#SBATCH --time=10:00:00             # Time limit hrs:min:sec
#SBATCH --output=array_%A-%a.out    # Standard output and error log
#SBATCH --array=1-7                # Array range
#SBATCH -n 1                # Array range

date
hostname

ml R

Rscript Run_Analysis_Sequential_mean.R ${SLURM_ARRAY_TASK_ID}

wait