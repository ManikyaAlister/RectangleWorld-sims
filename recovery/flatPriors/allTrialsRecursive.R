library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations
#alpha <- commandArgs(trailingOnly = TRUE) 
alpha <- -1
trials = 4
n = 100
prior = "flat"

allRects <- NULL
for (i in 1:trials){
  rects <- simulateLearnerGuesses(observations = observations, alpha = alpha, trial = i, nRectangles = n, prior = prior, recursion = TRUE)
  rects$trial = i
  allRects <- rbind(allRects, rects)
}

posteriors = NULL
for (i in 1:trials){
  rects = filter(allRects, trial == i)
  trial_posteriors <- getMultiAlphaPosteriors(learnerRectangles = rects, observations = observations, prior = prior, nTrials = 1, recursion = TRUE)
  trial_posteriors$trial = i
  posteriors = rbind(posteriors, trial_posteriors)
}

save(posteriors, file = here(paste0("recovery/data/a",alpha,"Alln",100,"_",prior,"-recursive.RData")))
