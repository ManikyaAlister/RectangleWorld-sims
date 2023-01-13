library(here)
source(here("experiment-scenarios/simulateModelFit.R"))
# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations

a1n100o4pr25_rects <- simulateLearnerGuesses(observations, alpha = 1, trial = 2, nRectangles = 100, prior = "normal")
a1n100o4pr25_posteriors <- getMultiAlphaPosteriors(learnerRectangles = a1n100o4pr25_rects, observations, prior = "normal")

save(a1n100o4pr25_posteriors, file = here("experiment-scenarios/simulate-distributions/simulated-learners/data/a1n100o4pr25.RData"))