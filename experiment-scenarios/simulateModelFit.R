rm(list = ls())
library(here)
library(ggplot2)
source(here("getLearnerHypDistributions.R"))

# Simulate learner guesses  -----------------------------------------------


# Sample rectangles at the probability predicted for a given alpha learner
simulateLearnerGuesses = function(observations, alpha, trial, nRectangles, prior = "normal"){
  # Get hypothesis (rectangle) distribution for a given set of clues and a given alpha
  dist <- getLearnerHypDistribution(observations[1:trial,], alpha = 1, nTrials = 1, prior = prior)
  dist <- dist[[1]]
  # Sample from rectangles
  sampleIndexes <- sample(dist$index, size = nRectangles, prob = dist$posterior, replace = TRUE)
  # Get the coordinates of the rectangles
  sampleRects <- NULL
  for (i in 1:nRectangles){
    index <- sampleIndexes[i]
    # Get the row of coordinates corresponding to the dampled index
    sampleCoords <- dist[dist[,"index"] == index, c("x1","y1","x2","y2")]
    # Combine into data frame
    sampleRects <- rbind(sampleRects,sampleCoords)
  }
  #return
  sampleRects <- cbind(sampleRects,sampleIndexes)
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
