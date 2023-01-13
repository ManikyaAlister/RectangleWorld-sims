library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

aNeg1n100o3prFlat_rects <- simulateLearnerGuesses(observations, alpha = -1, trial = 3, nRectangles = 100, prior = "flat")
aNeg1n100o3prFlat_posteriors <- getMultiAlphaPosteriors(learnerRectangles = aNeg1n100o3prFlat_rects, observations, prior = "flat")

save(aNeg1n100o3prFlat_posteriors, file = here("recovery/data/aNeg1n100o3prFlat.Rdata"))