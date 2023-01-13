library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

a1n100o1prFlat_rects <- simulateLearnerGuesses(observations = observations, alpha = 1, trial = 1, nRectangles = 100, prior = "flat")
a1n100o1prFlat_posteriors <- getMultiAlphaPosteriors(learnerRectangles = a1n100o1prFlat_rects, observations = observations, prior = "flat")

save(a1n100o1prFlat_posteriors, file = here("recovery/data/a1n100o1prFlat.RData"))