rm(list = ls())
library(here)
source(here("functions/generateExperimentBlock.R"))
# Script to generate a bunch of different experiment block scenarios for a given teacher, to try and 
# narrow down which ones to use for the experiment. 

# Configure block parameters ----------------------------------------------

# Set X and Y range for rectangle grid
H <- 10
# Size of the true rectangle
trueRectSize <- "small"
# Teacher's alpha
tchAlpha <- 1
# Alpha the teacher thinks the learner thinks the teacher has
tchLnAlpha <- 1
# learner Alpha 
lnAlpha <- 1
# Number of best hypotheses plotted
nBestH <- 3
# Configure whether the teacher is choosing the best point or sampling proportional to distribution
maximise <- TRUE
# set prior
prior <- "normal"
# set experiment scenario
experimentScenario <- "custom1"


# Generate block scenario -------------------------------------------------
debugonce(createExperimentBlock)
block <- createExperimentBlock(trueRectSize = trueRectSize, tchAlpha = tchAlpha, tchLnAlpha = 1, prior = prior, scenarioCode = experimentScenario, lnAlpha = lnAlpha, rect = c(2,2,6,6))
block
