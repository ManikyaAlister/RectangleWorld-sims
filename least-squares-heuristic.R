library(here)
setwd(here())

source("functions.R")

# weakLearner function updates the likelihood of each hypothesis given weak sampling
weakLearner = function(borders, observations) {
  isInRectPos = areInCat(borders, observations, "positive")
  areInRect = function(isInRect)
    setequal(isInRect, rep(TRUE, length(as.data.frame(isInRect))))
  positiveEvidence = if (length(as.data.frame(isInRectPos)) > 1) {
    # tells us which rectangles contain all of the observations of the category of interest
    apply(isInRectPos, 1, areInRect)
  } else {
    # if length is 1, no need to do the above function
    positiveEvidence = isInRectPos
  }
  
  isInRectNeg = areInCat(borders, observations, "negative") # does the hypothesised rectangle contain negative evidence?
  negativeEvidence = if (length(as.data.frame(isInRectPos)) > 1) {
    negativeEvidence = rowSums(isInRectNeg) > 0
  } else {
    negativeEvidence = isInRectNeg
  }
  
  hypotheses = cbind(borders, positiveEvidence, negativeEvidence)
  hypotheses = hypotheses[hypotheses[, "positiveEvidence"] == TRUE &
                            hypotheses[, "negativeEvidence"] == FALSE, ]
  hypotheses$likelihood = 1
  hypotheses$posterior = hypotheses[,"likelihood"]/sum(hypotheses[,"likelihood"]) # no prior yet so posterior is just likelihood over sum(likelihood)
  return(hypotheses)
}


#***** Least squares heuristic

# 1. Identify consistent hypotheses 
lsLearner = function(borders, obs) {
  consistent =  weakLearner(borders, obs)
  
  eDistance = function(point1, point2) {
    # euclidean distance formula: d = √[(x2 – x1)^2 + (y2 – y1)^2]
    sqrt((point2[1] - point1[1]) ^ 2 + (point2[2] - point1[2]) ^ 2)
  }
  
  
  #2 Rank hypotheses based on each point's proximity to the corners.
  #a. determine the coordinates of all of the corners:
  # For each eligible hypothesis, calculate all of the corners.
  meanDistances = NULL
  for (i in 1:length(consistent[, 1])) {
    # 1. get the coordinates of all of the corners
    xcoords = c(consistent[i, 1], consistent[i, 3])
    ycoords = c(consistent[i, 2], consistent[i, 4])
    corners = expand.grid(xcoords, ycoords)
    # 2. calculate the euclidean distance of each observation to each of the corners of each consistent hypothesis
    # - What is the distance from each observation to each corner, and then take the distance to its closest corner.
    distances = c()
    for (j in 1:length(obs[, 1])) {
      obsDistances = eDistance(corners, as.numeric(obs[j, 1:2]))
      minDistance = min(obsDistances)
      distances[j] = minDistance
    }
    meanDistances[i] = mean(distances) # average distance from each point to its nearest corner
  }
  
  hypotheses = cbind(consistent[1:4], meanDistances)
  colnames(hypotheses) = c("x1", "y1", "x2", "y2", "meanDistances")
  hypotheses$normalised = hypotheses$meanDistances / sum(hypotheses$meanDistances) # normalize so they are all between 0 and 1
  hypotheses$prob = (1 - hypotheses$normalised) / sum(1 - hypotheses$normalised) # Take from 1 so that the hypotheses with smallest distances have the highest probability
  return(hypotheses)
}
# plot

plotLS = function(hypotheses,observations, categoryBoundary, nHypotheses = "all",range = 1:10) {
  
  if (nHypotheses == "all"){
    nHypotheses = length(hypotheses[,1])
  } else {
    nHypotheses = nHypotheses
  }
  
  hypotheses = hypotheses[order(hypotheses[,"prob"], decreasing = TRUE),]
  hypotheses = hypotheses[1:nHypotheses,]
  hypotheses[,"prob"] = hypotheses[,"prob"]/sum(hypotheses[,"prob"])
  
  plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
  rect(hypotheses[,1],hypotheses[,2],hypotheses[,3],hypotheses[,4], col= rgb(0,0,1,alpha=hypotheses[,"prob"]),lwd = 0.01)
  #) # making alpha equal t 1/the number of hypotheses makes the transparency of the plot equivilent to the relitive probability of each hypothesis. 
  points(observations)
  rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "red", lwd = 2)
}

