library(here)
setwd(here())

source("functions.R")

# #***** Set up
# 
# # set up rectangle world
# 
# # Define range of possible features
# range = 1:10
# 
# # Create array with all possible rectangle coordinates within the range 
# borders = expand.grid(range,range,range,range)
# 
# for (i in 1:length(borders[,1])){ # replace duplicate rectangles with NA
#   if (borders[i,1]-borders[i,3] > 0 | borders[i,2]-borders[i,4] > 0){
#     borders[i,] = NA
#   }
# }
# 
# borders = borders[complete.cases(borders),] # delete rows that previously held duplicate rectangles (there was probably a better way to do this)
# 
# # Define the true category region
# cat1 = c(2,2,6,8)
# 
# #visualize true category region
# plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
# 
# rect(cat1[1],cat1[2],cat1[3],cat1[4],border = "blue", lwd = 3)
# 
# 
# # Sample observations
# 
# nObs = 10
# # Positive examples
# pX = round(runif(nObs/2, cat1[1],cat1[3]),2) # X coordinates 
# pY = round(runif(nObs/2, cat1[2],cat1[4]),2) # Y coordinates
# pos = cbind(pX,pY,"1")
# 
# ## Need to find a  better way to sample negative evidence 
# neg = weakSampler(20)
# neg = neg[neg[,"category"]== "none",]
# neg = neg[1:5,]
# 
# # set up different arrays with different numbers of observations
# obs1 = rbind(pos[1,],neg[1,])
# colnames(obs1) = c("x","y","category")
# 
# obs3 = rbind(pos[1:3,],neg[1:3,])
# colnames(obs3) = c("x","y","category")
# 
# obs5 = rbind(pos[1:5,],neg[1:5,])
# colnames(obs5) = c("x","y","category")
# 
# # visualize observations
# plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "", main = "Hypothesis Space, Observations, and True Category Boundary")
# rect(cat1[1],cat1[2],cat1[3],cat1[4],border = "blue", lwd = 3)
# points(obs5)

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

plotLS = function(hypotheses,observations, categoryBoundary, range = 1:10) {
  plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
  rect(hypotheses[,1],hypotheses[,2],hypotheses[,3],hypotheses[,4], col= rgb(0,0,1,alpha=hypotheses[,"prob"]),lwd = 0.01)
  #) # making alpha equal t 1/the number of hypotheses makes the transparency of the plot equivilent to the relitive probability of each hypothesis. 
  points(observations)
  rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "darkblue", lwd = 3)
}

#test5 = lsLearner(borders,obs5) 
#plotLS(hypotheses,obs5,cat1)

#tmp = hypotheses[2,]
#plotLS(tmp,obs5,cat1)
