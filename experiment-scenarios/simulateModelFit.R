rm(list = ls())
library(here)
library(ggplot2)
source(here("getLearnerHypDistributions.R"))

# Simulate learner guesses  -----------------------------------------------

# For target block 1
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
observations = targetTrial1$observations


# Sample rectangles at the probability predicted for a given alpha learner
simulateLearnerGuesses = function(observations, alpha, trial, nRectangles, prior = "normal"){
  # Get hypothesis (rectangle) distribution for a given set of clues and a given alpha
  dist <- getLearnerHypDistribution(observations[1:trial,], alpha = 1, nTrials = 1, prior = prior)
  dist <- dist[[1]]
  # Sample from rectangles
  sampleIndexes <- sample(dist$index, size = nRectangles, prob = dist$posterior, replace = TRUE)
  # Get the coordinates of the rectangles
  sampleRects <- dist[sampleIndexes, c("x1","y1","x2","y2")]
  #return
  sampleRects
}

# Get posterior probability of multiple learner guesses under different alphas 
getMultiAlphaPosteriors = function(learnerRectangles,
                                   observations,
                                   nTrials = 1,
                                   alphasToSearch = "all-alphas",
                                   H = 10,
                                   prior = "normal") {
  allPosteriors <- NULL
  for (i in 1:length(learnerRectangles[, 1])) {
    rect <- as.vector(learnerRectangles[i, ])
    posteriors <-
      rectangleAlphaPosteriors(
        learnerRectangle = rect,
        observations = observations,
        nTrials = nTrials,
        alphasToSearch = alphasToSearch,
        H = H,
        prior = prior
      )
    allPosteriors <- rbind(allPosteriors, posteriors)
  }
  
  allPosteriors <- as.data.frame(allPosteriors)
  allPosteriors
}



a1n50o2pr25_rects <- simulateLearnerGuesses(observations, alpha = 1, trial = 2, nRectangles = 100, prior = "normal")
a1n50o2_pr25posteriors <- getMultiAlphaPosteriors(learnerRectangles = a1n50o2_rects, observations, prior = "normal")
