library(jsonlite)
rm(list = ls())
source(here("functions/simulateMultipleAlphas.R"))

targetRect1 <- c(5,2,9,6)
targetRect2 <- 10-targetRect1
targetRect2 <- c(targetRect2[3:4],targetRect2[1:2]) # flip the order so smallest coordinate pair at the front

# Convert from Cartesian to grid format (for experiment)
targetGridRect1 <- c(targetRect1[1]+1, (10-targetRect1[2]), targetRect1[3], (10-targetRect1[4])+1)
targetGridRect2 <- c(targetRect2[1]+1, (10-targetRect2[2]), targetRect2[3], (10-targetRect2[4])+1)

# Make target observations
observationsT1 <- data.frame(x = c(5.5, 8.5,5.5,9.5), y = c(2.5,5.5,5.5,3.5), category = c("positive","positive","positive","negative"))

# Second target trial is just the first target trial inverted
observationsT2 <- observationsT1 %>%
  mutate(x = 10-x,
         y = 10-y)


# Convert to experiment format  (grid structure instead of Cartesian structure)
gridObsT1 <- observationsT1 %>%
  mutate(x = x+0.5,
         y = (10-y)+0.5)

gridObsT2 <- observationsT2 %>%
  mutate(x = x+0.5,
         y = (10-y)+0.5)

# Combine all block data into list 
targetTrial1 = list("groundTruth" = targetRect1, "observations" = observationsT1)
targetTrialGrid1 = list("groundTruth" = targetGridRect1, "observations" = gridObsT1)

targetTrial2 = list("groundTruth" = targetRect2, "observations" = observationsT2)
targetTrialGrid2 = list("groundTruth" = targetGridRect2, "observations" = gridObsT2)

save(targetTrial1, file = here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
save(targetTrial2, file = here("experiment-scenarios/target-blocks/data/target-trial-2-Cartesian.Rdata"))

# Plot

# Size of rectangle world grid. 
H <- 10

# All learner alpha predictions to test 
allAlphas <- c(1,0.5,0.1,0,-0.1, -0.5, -1)

# visualise target blocks
#debugonce(multiAlphaPredictions)
target1 <- multiAlphaPredictions(observationsT1, save = TRUE, allAlphas = c(-1,1))
target1
target2 <- multiAlphaPredictions(observationsT2, save = TRUE)
target2

# saving with as generic name -- necessary for model fits (todo if time: make this less hacky)
targetBlock <- targetTrial1
save(targetBlock, file = here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))

targetBlock <- targetTrial2
save(targetBlock, file = here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))


