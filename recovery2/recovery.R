# need to deactivate renv on the super computer
#renv::deactivate()
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(dplyr, lib.loc = lib)
library(stringr, lib.loc = lib)
source(here("getLearnerHypDistributions.R"))

# run locally and parallel:
  # run-recovery.sh
# run on HPC and parallel: 
  # run recovery.sbatch

# Retrieve the values of alpha and clue from command-line arguments (configure in run-recovery script)
# args <- commandArgs(trailingOnly = TRUE)
# alpha <- as.numeric(args[1])
# clue <- as.numeric(args[2])
alpha <- 1
clue <- 4
# configure as per requirements
nRectangles = 20
prior = "flat"
recursion = FALSE

# print configurations to terminal so I know what is running:  
print(paste0("alpha: ", alpha))
print(paste0("clue: ", clue))
print(paste0("recursion: ", recursion))

# load experiment observations
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

rects <- simulateLearnerGuesses(observations = observations, alpha = alpha, trial = clue, nRectangles = nRectangles, prior = prior, recursion = recursion)

posteriors <- getMultiAlphaPosteriors(learnerRectangles = rects, observations = observations, prior = prior, recursion = recursion, nTrials = clue)

if (recursion) {
  save(posteriors, file = here(paste0("recovery2/data/a",alpha,"_n",nRectangles,"_c",clue,"_pr-",prior,"_recursion.RData")))
  } else {
    save(posteriors, file = here(paste0("recovery2/data/a",alpha,"_n",nRectangles,"_c",clue,"_pr-",prior,".RData")))
}
