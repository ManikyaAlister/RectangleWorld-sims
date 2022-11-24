#### Pedagogical teacher 
library(here)
library(roxygen2)
source(here("functions.R"))
source(here("pedagogical-learner.R"))
root = here()

#' @title Pedagogical Teacher
#' @description 
#' pedTeacher chooses points to teach a learner where the true rectangle is
#' @example 
#' pedTeacher(trueRectangle = c(2,2,6,6), borders = makeBorders(1:10), nPoints = 3)
#' 
#' @param trueRectangle A vector of coordinates indicating the true rectangle that the 
#' @param borders Array of hypothesised category boundary points (coordinates of rectangles). Can be generated using the makeBorders() function. 
#' @param nPoints Number indicating how many points you want the teacher to provide.
#'
pedTeacher = function(trueRectangle, 
                      borders, 
                      nPoints,  
                      range = 1:10,
                      alpha = 1){ 
  
  # set up all possible points
  allPoints = expand.grid(range,range) 
  
  # label points as either positive or negative evidence
  f = function(p) isInRectangle(r = trueRectangle, p = p) # alter isInRectangle function to be appropriate for apply() (include true rectangle)
  positiveEvidence = apply(allPoints,1,f) # tells us if a point is in the rectangle or not
  category = rep(NA,length(allPoints)) # empty column to fill with labels
  observations = cbind(allPoints,category) # join into 1 matrix
  
  for (i in 1:length(positiveEvidence)){
    if (positiveEvidence[i] == TRUE){
      observations[i,"category"] = "positive"
    } else {
      observations[i,"category"] = "negative"
    }
  }
  
  # See which points maximize the pedagogical learner's posterior probability that the true rectangle is the best rectangle relative to the other rectangle
  
  # set up array to fill with priors
  learnerPrior = array(NA, dim = c(length(borders[,1]),nPoints+1)) # nPoints "+ 1" because there needs to be space for the podsterior of the 3rd point + 1
  
  # Set the learner's initial prior
  learnerPrior[,1] = rep(1,length(borders[,1]))/sum(rep(1,length(borders[,1]))) # uniform prior to start
  
  # empty vector to fill with the points that the teacher chooses
  points = array(NA,c(nPoints,2)) 
  
  # loop through each point that the teacher is providing
for (j in 1:nPoints){ 

  # set up an empty data frame to fill with the probability of the true rectangle given each point
  probTrueRect = c()
  
  # Calculate the learner's posterior for each hypothesis for each point:
  for (i in 1:length(observations[,1])){
    observation = as.vector(as.matrix(observations[i,])) # needs to be a vector for pedLearner, but as.vector() wouldn't work unless it was first converted to a matrix. Not sure why. 
    names(observation) = c("Var1","Var2","category") # needs to be named vector for pedLearner to work 
    learner = pedLearner(borders,observation, prior = learnerPrior[,j], alpha = alpha) 
    pointProb = learner[learner[,1] == trueRectangle[1] & learner[,2] == trueRectangle[2] & learner[,3] == trueRectangle[3] & learner[,4] == trueRectangle[4],"posterior"] # extract just the probability of the true rectangle for each  
    probTrueRect[i] = pointProb 
    }
  
  # choose the point with the largest probability
  maxProb = which.max(probTrueRect) # <-- seems like it's giving the same probability for every positive point and every negative point. 
  points[j,] = as.matrix(observations[maxProb,1:2])
  learnerPrior[,j+1] = learner[,"posterior"] # the learner's prior about the rectangles for the next point is their posterior from the last point
  observations = observations[-maxProb,]
  }
  
  return(points)
}

#' @title Plot the Output of pedTeacher
#' @description
#' Plot the points chosen by a pedagogical teacher
#' @example 
#' plotPedTeacher(points = pedTeacher(trueRectangle = c(2,2,6,6), borders = makeBorders(1:10), nPoints = 10), trueRectangle = c(2,2,6,6),borders = makeBorders(1:10))
#' @param trueRectangle Numeric vector with coordinates of the true rectangle. 
#' @param points Numeric array with the chosen points of the teacher ordered correctly. Output from pedTeacher()

plotPedTeacher = function(points, 
                          trueRectangle, 
                          range = 1:10){
  plot(c(1,max(range)), c(1, max(range)), type = "n", xlab = "", ylab = "", main = "")
  rect(trueRectangle[1],trueRectangle[2],trueRectangle[3],trueRectangle[4],border = "red", lwd = 2) 
  points(points, cex = 1.25) 
  for (i in 1:length(points[,1]))
  text(x = points[i,1],y = points[i,2], label = i , cex = 0.5)
}

roxygen2::roxygenise()
