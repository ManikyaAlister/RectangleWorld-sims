library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

alpha <- commandArgs(trailingOnly = TRUE) 
trials = 4
n = 100

allRects <- NULL
for (i in 1:trials){
  rects <- simulateLearnerGuesses(observations = observations, alpha = alpha, trial = i, nRectangles = n, prior = "flat")
  allRects <- rbind(allRects, rects)
}

posteriors <- getMultiAlphaPosteriors(learnerRectangles = allRects, observations = observations, prior = "flat", nTrials = 4)
save(posteriors, file = here(paste0("recovery/data/a",alpha,"Alln",100,"Flat.RData")))