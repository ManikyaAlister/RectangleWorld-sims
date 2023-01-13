library(here)
source(here("getLearnerHypDistributions.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

a0n100o2pr25_rects <- simulateLearnerGuesses(observations = observations, alpha = 0, trial = 2, nRectangles = 100, prior = "normal")
a0n100o2pr25_posteriors <- getMultiAlphaPosteriors(learnerRectangles = a0n100o2pr25_rects, observations, prior = "normal")

save(a0n100o2pr25_posteriors, file = here("recovery/data/a0n100o2pr25.RData"))