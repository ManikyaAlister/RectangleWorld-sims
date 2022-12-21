rm(list = ls())
library(here)
source(here("generateExperimentBlock.R"))
# Explore the block scenarios chosen for the experiment. 

# Configure block parameters ----------------------------------------------

# Set X and Y range for rectangle grid
H <- 10
# Size of the true rectangle
trueRectSize <- "medium"
# Teacher's alpha
tchAlpha <- 1
tchLnAlpha <- 1
# Learner's actual alpha for the teacher (participant predictions)
lnAlpha <- 1
# Number of best hypotheses plotted
nBestH <- 3
# Configure whether the teacher is choosing the best point or sampling proportional to distribution
maximise <- TRUE
# set prior
prior <- "normal"

# load data 
fileSeg <- paste0("x0to",H,"y0to",H)
fn <- paste0("datafiles/",fileSeg,".RData")
load(here(fn))

# vector of rectangles chosen for scenarios currently (manually chosen)
experimentRectanglesIndex <- c(645, 1774, 1778, 2315, 1069, 1855, 365, 1792)
experimentRectangles <- hyp[experimentRectanglesIndex, 1:4]

# Figure out experimental condition 
if(lnAlpha > 0){
  cond <- "Trusting-learner"
} else if (lnAlpha == 0){
  cond <- "Weak-learner"
} else if (lnAlpha < 0) {
  cond <- "Suspicious-learner"
}

# Loop through all blocks 
for (i in 1:length(experimentRectanglesIndex)){
# choose block rectangle from all scenario rectangles 
rect <- as.vector(as.matrix(experimentRectangles[i,])) 

# Configure scenario code for saving 
#scenarioCode <- paste0(i,"-",experimentRectanglesIndex[i],"-tchA",tchAlpha, "-lnA",lnAlpha,"tchLnA",tchLnAlpha,"-prior-",prior)
scenarioCode <- paste0(i,"-",experimentRectanglesIndex[i],"-tchA",tchAlpha)

block <- createExperimentBlock(trueRectSize = trueRectSize, tchAlpha = tchAlpha, rect = rect, prior = prior, lnAlpha = lnAlpha, tchLnAlpha = tchLnAlpha, scenarioCode = scenarioCode)
}
