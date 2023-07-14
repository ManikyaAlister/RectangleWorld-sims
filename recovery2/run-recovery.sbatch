#!/bin/bash
#SBATCH --job-name="recovery"
#SBATCH --array=1-12  # Number of combinations: (alphas) * (clues)
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=1-1:0:00

# The amount of memory in megabytes per process in the job:
#SBATCH --mem=2G

# Use this email address:
#SBATCH --mail-user=alisterm@student.unimelb.edu.au

# Send yourself an email when the job:
# ends successfully
#SBATCH --mail-type=END

# Where to save output logs 
#SBATCH --output=slurm_output/%A_%a.out
#SBATCH --error=slurm_output/%A_%a.err

# Define the values for alpha and clue
alphas=(1 0 -1)
clues=(1 2 3 4)

# Calculate the array index
array_index=$((SLURM_ARRAY_TASK_ID - 1))
alpha_index=$((array_index / 4)) # number of clues after "/"
clue_index=$((array_index % 4)) # number of clues after "%"

# Assign the values of alpha and clue based on the indices
alpha=${alphas[$alpha_index]}
clue=${clues[$clue_index]}

# The modules to load:
module load r/4.2.0

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s

# Execute the R script with the current alpha and clue values
Rscript recovery2/recovery.R $alpha $clue
