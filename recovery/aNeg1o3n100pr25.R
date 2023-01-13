library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

aNeg1n100o3pr25_rects <- simulateLearnerGuesses(observations, alpha = -1, trial = 3, nRectangles = 100, prior = "normal")
aNeg1n100o3pr25_posteriors <- getMultiAlphaPosteriors(learnerRectangles = aNeg1n100o3pr25_rects, observations, prior = "normal")

save(aNeg1n100o3pr25_posteriors, file = here("recovery/data/aNeg1n100o3pr25.Rdata"))