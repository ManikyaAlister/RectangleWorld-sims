#!/bin/bash
#SBATCH --job-name="rec-recur"
#SBATCH --array=1-288  # Number of combinations: (alphas) * (clues) * (providers) * (blocks)
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

# Define the values for alpha, clue, provider, and block
alphas=(1 0 -1)
clues=(1 2 3 4)
providers=("helpful" "random" "misleading" "uninformative")
blocks=(1 3 4 5 6 7)

# Calculate the array indices
array_index=$((SLURM_ARRAY_TASK_ID - 1))
alpha_index=$((array_index / (4 * 4 * 6))) # (number of clues after "/") * (number of providers after "/") * (number of blocks after "/")
clue_index=$(( (array_index % (4 * 4 * 6)) / (4 * 6) )) # (number of clues after "%") * (number of blocks after "/")
provider_index=$(( (array_index % (4 * 4 * 6)) / 6 % 4 )) # (number of blocks after "%")
block_index=$((array_index % 6)) # number of blocks after "%"

# Assign the values of alpha, clue, provider, and block based on the indices
alpha=${alphas[$alpha_index]}
clue=${clues[$clue_index]}
provider=${providers[$provider_index]}
block=${blocks[$block_index]}

# The modules to load:
module load r/4.2.0

# Execute the R script with the current alpha, clue, provider, and block values
Rscript recovery2/recovery.R $alpha $clue $block $provider

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s
