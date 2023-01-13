rm(list = ls())
# Simulate target trials --------------------------------------------------
# source simulation function
source(here("simulateExperimentBlock.R"))
# load data 
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
load(here("experiment-scenarios/target-blocks/data/target-trial-2-Cartesian.Rdata"))


# Target trial 1 ----------------------------------------------------------

# Ground truth rectangle
trueHT1 <- targetTrial1$groundTruth

# Observations
obsT1 <- targetTrial1$observations

# Plot heat map of distribution
t1_trusting <- simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = 1, prior = "normal")
t1_trusting
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_trusting.png"), width = 10)


t1_random <- simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = 0, prior = "normal")
t1_random
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_random.png"), width = 10)


t1_suspicious <-simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = -1, prior = "normal")
t1_suspicious
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_suspicious.png"), width = 10)



