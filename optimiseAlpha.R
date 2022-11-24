source("pedagogical-learner.R")
source("functions.R")
source("samplingFunctions.R")

# step 1: what rectangle does a given alpha create for a set of observations?

borders = makeBorders(0:10)

#' Grid search through a range of alpha values
#'
#' @param alphaRange vector of alpha values you want to search
#' @param borders entire range of possible rectangles
#' @param observations 3-columned labeled matrix of points provided by the teacher
#' @param prior prior probability of each rectangle
#'
#' @return 5 by length(alphaRange) array with rectangle coordinates and the alpha that generated those coordinates in each row. 
#' @export
#'
#' @examples
alphaGridSearch = function(alphaRange = seq(from = -1, to = 1, by = .2),
                           borders,
                           observations,
                           prior = "uniform") {

    gridSearch = array(NA, dim = c(length(alphaRange), 4)) # create an empty array to fill with the coordinates of each rectangle predicted by a given alpha.
  
  for (i in 1:length(alphaRange)) {
    rect =  pedLearner(borders, observations, prior, alpha = alphaRange[i]) # generate all eligible rectangles (hypotheses)
    orderedRect =  order(rect[, "posterior"], decreasing = TRUE) # order from highest to lowest probability
    bestRect = as.numeric(rect[orderedRect[1], 1:4]) # take the most probable rectangle, keeping only the coordinate information (columns 1:4)
    gridSearch[i,] = bestRect # fill relevant row with the rectangle for that alpha
  }
  
  gridSearch = cbind(gridSearch, alphaRange) # add a column so we know what alpha generated each rectangle
  colnames(gridSearch) = c("x1", "y1", "x2", "y2", "alpha") # name
  return(gridSearch)
  
}

# Step 2: Create a function that calculates the similarity of one rectangle to another. 

#' Find the area of overlap between two rectangles
#'
#' @param rect1 coordinates of first rectangle, e.g., c(<x1>, <y1>, <x2>, <y2>)
#' @param rect2 coordinates of second rectangle 
#'
#' @return area of overlap between the two rectangles
#'
#' @examples rectOverlap(rect1 = c(2,2,6,6), rect2 = c(4,4,8,8))
rectOverlap = function(rect1, rect2) { 

  # figure out which x and y values are the maximum and minimum values of each rectangle 
  x1Max = max(c(rect1[1], rect1[3]))
  x1Min = min(c(rect1[1], rect1[3]))
  y1Max = max(c(rect1[2], rect1[4]))
  y1Min = min(c(rect1[2], rect1[4]))
  
  x2Max = max(c(rect2[1], rect2[3]))
  x2Min = min(c(rect2[1], rect2[3]))
  y2Max = max(c(rect2[2], rect2[4]))
  y2Min = min(c(rect2[2], rect2[4]))
 
  # equation for calculating the area of the overlap 
  dx = min(x1Max, x2Max) - max(x1Min, x2Min)
  dy = min(y1Max, y2Max) - max(y1Min, y2Min)
  
  if (dx >= 0 & dy >= 0) {
    size = dx * dy
  } else {
    size = 0
  }
  return(size)
}  

# Step 3: Make a function that runs through each participant and finds the alpha that best describes them. 

# Test 

plotAlphaPredictions = function(rects, obs, trueRect) {
  rects = as.data.frame(rects)
  rects$alpha = as.factor(rects$alpha)
  obs = as.data.frame(obs)
  colnames(obs) = c("x", "y", "category")
  obs$x = as.numeric(obs$x)
  obs$y = as.numeric(obs$y)
  
  ggplot(data = rects) +
    geom_rect(aes(
      xmin = rects[, 1],
      ymin = rects[, 2],
      xmax = rects[, 3],
      ymax = rects[, 4],
      fill = alpha
    ),
    alpha = 1) +
    geom_point(data = obs[obs[, "category"] == "positive", ], aes(x = x, y = y), colour = "black") +
    geom_point(data = obs[obs[, "category"] == "negative", ], aes(x = x, y = y), colour = "red") +
    geom_rect(
      aes(
        xmin = trueRect[1],
        ymin = trueRect[2],
        xmax = trueRect[3],
        ymax = trueRect[4]
      ),
      alpha = 0,
      colour = "black"
    )
}

## Idea for generating data: generate points and true rectangles randomly but set a 
# condition where only points that have a different rectangle for each alpha are chosen. 



# small rectangle
smallRect = c(6,6,8,8)
allObservations = weakSampler(30, smallRect)
obsPos = allObservations[allObservations[,"category"] == "positive",]
obsNeg = allObservations[allObservations[,"category"] == "negative",]

# Large rectangle 

largeRect = c(2,2,8,8)

obs = samplePosNeg(4,2,largeRect)
test1 = alphaGridSearch(borders = borders, observations = obs)
plotAlphaPredictions(rects = test1, obs = obs, largeRect)

obs2 = samplePosNeg(1,1,largeRect) 
test1 = alphaGridSearch(borders = borders, observations = obs2)
plotAlphaPredictions(rects = test1, obs = obs2, largeRect)

