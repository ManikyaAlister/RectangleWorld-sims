#### Pedagogical teacher 
library(here)
source(here("functions.R"))
source(here("pedagogical-learner.R"))

# 1. set up true rectangle 
cat1 = c(2,2,6,8)
trueRectangle = cat1
# 2. set up all possible points
range = 1:10
plot(test)

pedTeacher = function(trueRectangle, range, nPoints){
  
  # calculate all eligible possible rectangles within range
  borders = makeBorders(range)
  
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
  learnerPrior = array(NA, dim = c(length(borders[,1]),nPoints))
  
  # Set the learner's initial prior
  learnerPrior[,1] = rep(1,length(borders[,1]))/sum(rep(1,length(borders[,1]))) # uniform prior to start
  
  # loop through each point that the teacher is providing
  for (j in nPoints){

  
  # set up an empty data frame to fill with the probability of the true rectangle given each point
  probTrueRect = c()
  
  # Calculate the learner's posterior for each hypothesis for each point:
  for (i in 1:length(allPoints[,1])){
    observation = as.vector(as.matrix(observations[i,])) # needs to be a vector for pedLearner, but as.vector() wouldn't work unless it was first converted to a matrix. Not sure why. 
    names(observation) = c("Var1","Var2","category") # needs to be named vector for pedLearner to work 
    learner = pedLearner(borders,observation) 
    pointProb = learner[learner[,1] == trueRectangle[1] & learner[,2] == trueRectangle[2] & learner[,3] == trueRectangle[3] & learner[,4] == trueRectangle[4],"posterior"] # extract just the probability of the true rectangle for each  
    probTrueRect[i] = pointProb 
    }
  
  # choose the point with the largest probability
  maxProb = which.max(probTrueRect) # <-- seems like it's giving the same probability for every positive point and every negative point. 
  point[j] = allPoints[maxProb,]
  learnerPrior[,j+1] = learner[,"posterior"] # the learner's prior about the rectangles for the next point is their posterior from the last point
  
  }
  
  return(point)
}

borders = makeBorders(1:10)
observation = obs1Pos
trueRectangle 

test = pedTeacher(cat1,range)

test1 = pedLearner(borders,tmp)

#Var1 Var2 category
tmp = c(1,1, "negative")
names(tmp) = c("Var1","Var2","category")

plot(c(1,max(range)), c(1, max(range)), type = "n", xlab = "", ylab = "", main = "Hypothesis Space, Observations, and True Category Boundary")
rect(cat1[1],cat1[2],cat1[3],cat1[4],border = "red", lwd = 2)
