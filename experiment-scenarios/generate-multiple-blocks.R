rm(list = ls())
library(here)
source(here("generateExperimentBlock.R"))
source(here("genericFunctions.R"))
# Explore the block scenarios chosen for the experiment.



# Configure block parameters ----------------------------------------------

# Set X and Y range for rectangle grid
H <- 10
# Size of the true rectangle
trueRectSize <- "medium"
# Teacher's alpha
tchAlpha <- 0
tchLnAlpha <- 0
# Learner's actual alpha for the teacher (participant predictions)
lnAlpha <- 0
# Number of best hypotheses plotted
nBestH <- 3
# Configure whether the teacher is choosing the best point or sampling proportional to distribution
maximise <- TRUE
# set prior
prior <- "normal"
# set trial IDs
trialIDs <- c(3)

# Provider helpfulness condition
if (tchAlpha == 1 & lnAlpha == 1) {
  cond <- "helpful"
} else if (tchAlpha == 0 & tchLnAlpha == 0) {
  cond <- "random"
} else if (tchAlpha == -1 & tchLnAlpha == -1) {
  cond <- "uninformative"
} else if (tchAlpha == -1 & tchLnAlpha == 1) {
  cond <- "misleading"
}

# load data
fileSeg <- paste0("x0to", H, "y0to", H)
fn <- paste0("datafiles/", fileSeg, ".RData")
load(here(fn))

# vector of rectangles chosen for scenarios currently (manually chosen)

# rectangles <- list(
# c(2,3,7,6),
# c(2,2,8,8),
# c(5,2,8,8),
# c(2,2,9,9),
# c(2,3,9,6),
# c(8,3,9,8),
# c(3,3,4,4),
# c(4,4,8,8)
# )

rectangles <- list()
rectangles[[1]] <- c(1,1,9,8)

experimentRectanglesIndex <- getRectangleIndex(rectangle = rectangles, hyp = hyp, nRectangles = length(hyp[, 1]))

experimentRectangles <- hyp[experimentRectanglesIndex, 1:4]

# Figure out experimental condition
# if(lnAlpha > 0){
#   cond <- "Trusting-learner"
# } else if (lnAlpha == 0){
#   cond <- "Weak-learner"
# } else if (lnAlpha < 0) {
#   cond <- "Suspicious-learner"
# }

# Loop through all blocks
for (i in 1:length(experimentRectanglesIndex)) {
  # choose block rectangle from all scenario rectangles
  rect <- as.vector(as.matrix(experimentRectangles[i, ]))
  
  # Configure scenario code for saving
  scenarioCode <-
    paste0(trialIDs[i], "-", as.numeric(paste(rectangles[[i]], collapse = "")), "-", cond)

  block <-
    createExperimentBlock(
      trueRectSize = trueRectSize,
      tchAlpha = tchAlpha,
      rect = rect,
      prior = prior,
      lnAlpha = lnAlpha,
      tchLnAlpha = tchLnAlpha,
      scenarioCode = scenarioCode,
      trialIds = trialIDs
    )
}
