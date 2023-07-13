rm(list = ls())

#### Simulate Heatmaps ######
library(here)
library(ggpubr)
source(here("getLearnerHypDistributions.R"))
source(here("calculatingFunctions.R"))
#source(here("fixing_heatmaps.R"))
source(here("plottingFunctions.R"))

load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))

clue = 4

aPos = simulateLearnerGuesses(targetBlock$observations, alpha = 1, trial = clue, 100, prior = "flat")
aPos$cond = "HS"
aPos$clue = clue

a0 = simulateLearnerGuesses(targetBlock$observations, alpha = 0, trial = clue, 100, prior = "flat")
a0$cond = "RS"
a0$clue = clue

aNeg = simulateLearnerGuesses(targetBlock$observations, alpha = -1, trial = clue, 100, prior = "flat")
aNeg$cond = "MS"
aNeg$clue = clue

r_aNeg = simulateLearnerGuesses(targetBlock$observations, alpha = -1, trial = clue, 100, recursion = TRUE, prior = "flat")
r_aNeg$cond = "US"
r_aNeg$clue = clue


simData <- rbind(aPos, a0, aNeg, r_aNeg)
simData$block <- 8
trueR <- targetBlock$groundTruth
simData$ground_truth_x1 <- trueR[1]
simData$ground_truth_y1 <- trueR[2]
simData$ground_truth_x2 <- trueR[3]
simData$ground_truth_y2 <- trueR[4]

# load condition labels
load(here("experiment-1/data/derived/all_conditions.R"))

blocks <- 8

all_conditions <- all_conditions %>% 
  filter(conditions %in% c("HS", "RS", "MS", "US") & clues %in% c(clue) & blocks ==8)
           

# get hypothesis probabilities of each condition 
#getPtProbs(d = simData, all_conditions = all_conditions, experiment = "sim")

getHypProbs(d = simData, all_conditions = all_conditions, experiment = "sim")



# Plot heat maps
plotHeatMaps(all_conditions = all_conditions, experiment = "sim")

# 
# load(here("experiment-1-rerun/data/derived/data_cartesian.Rdata"))
# 
# getHypProbs(d = d_cartesian, all_conditions = all_conditions, experiment = "1")
# 
# plotHeatMaps2(all_conditions = all_conditions, experiment = "1")
# 
# source(here("getLearnerHypDistributions.R"))
# test <- plotLearnerHypDistribution(targetBlock$observations, prior = "flat")

#hyp <- getLearnerHypDistribution(obs = targetBlock$observations,  )
#pts <- expand.grid(x = seq(0.5,9.5,1), y = seq(0.5,9.5,1))
#probPts <- findProbabilityOfPoints(hyp[[1]], pts = pts, whichObs = "pos") 
#probPts <- updatePoints()
#debugonce(plotDistribution)
#plotDistribution(obs = targetBlock$observations, trueRectangle = targetBlock$groundTruth, allPts = probPts)
