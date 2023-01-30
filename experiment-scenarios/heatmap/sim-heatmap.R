rm(list = ls())

#### Simulate Heatmaps ######
library(here)
source(here("getLearnerHypDistributions.R"))

load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))

a1c1 = simulateLearnerGuesses(targetBlock$observations, alpha = 2, trial = 1, 100)
a1c1$cond = "HS"
a1c1$clue = 1

#debugonce(simulateLearnerGuesses)
a1c4 = simulateLearnerGuesses(targetBlock$observations, alpha = 2, trial = 4, 100)
a1c4$cond = "HS"
a1c4$clue = 4


a0c1 = simulateLearnerGuesses(targetBlock$observations, alpha = 0, trial = 1, 100)
a0c1$cond = "RS"
a0c1$clue = 1


a0c4 = simulateLearnerGuesses(targetBlock$observations, alpha = 0, trial = 4, 100)
a0c4$cond = "RS"
a0c4$clue = 4


aNeg1c1 = simulateLearnerGuesses(targetBlock$observations, alpha = -2, trial = 1, 100)
aNeg1c1$cond = "MS"
aNeg1c1$clue = 1

aNeg1c4 = simulateLearnerGuesses(targetBlock$observations, alpha = -2, trial = 4, 100)
aNeg1c4$cond = "MS"
aNeg1c4$clue = 4

simData <- rbind(a1c1, a1c4, a0c1, a0c4, aNeg1c1, aNeg1c4)
simData$block <- 8
trueR <- targetBlock$groundTruth
simData$ground_truth_x1 <- trueR[1]
simData$ground_truth_y1 <- trueR[2]
simData$ground_truth_x2 <- trueR[3]
simData$ground_truth_y2 <- trueR[4]


# load condition labels
load(here("experiment-1/data/derived/all_conditions.R"))


all_conditions <- all_conditions %>% 
  filter(conditions %in% c("HS", "RS", "MS") & clues %in% c(1,4) & targetBlocks == 8)


# Plot heat maps
plotHeatMaps(d = simData, all_conditions = all_conditions, experiment = "sim")

