#!/bin/bash

# set up job array (make sure the array length is nproviders * nblocks)
#SBATCH --array=1-24

# Partition for the job:
#SBATCH --partition=physical

# Multithreaded (SMP) job: must run on one node 
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="fits"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

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

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=7-1:0:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from the directory where it was launched (default)

# The modules to load:
module load r/4.2.0


# Set the provider script names
providers=("helpful" "random" "misleading" "uninformative")

# Set the block numbers (make sure the slurm array is set up for the correct provider * block combintation)
blocks=(1 3 4 5 6 7)

# Calculate the provider and block indices from the job array task ID
provider_idx=$(((SLURM_ARRAY_TASK_ID-1) % 4)) # make sure you correctly indicate the number of providers after '%'
block_idx=$(((SLURM_ARRAY_TASK_ID-1) / 4)) # make sure you correctly indicate the number of providers after '/'

# Assign the values of provider and block based on the indices
provider=${providers[$provider_idx]}
block=${blocks[$block_idx]}


# Run the R script with the input and output files and the block number as a command line argument
Rscript experiment-scenarios/hypothesis-distributions/get-learner-block-distributions.R $block $provider

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s

