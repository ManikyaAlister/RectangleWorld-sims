source("pedagogical-learner.R")
source("functions.R")
source("samplingFunctions.R")

# step 1: what rectangle does a given alpha create for a set of observations?

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
alphaGridSearch = function(borders,
                           observations,
                           alphaRange = seq(from = -1, to = 1, by = .2),
                           prior = "uniform") {

    gridSearch = NULL
  for (i in 1:length(alphaRange)) {
    rect =  pedLearner(borders, observations, prior, alpha = alphaRange[i]) # generate all eligible rectangles (hypotheses)
    orderedRect =  order(rect[, "posterior"], decreasing = TRUE) # order from highest to lowest probability
    bestRect = rect[rect[,"posterior"] == rect[orderedRect[1],"posterior"],1:4] # take the most probable rectangle, keeping only the coordinate information (columns 1:4)
    alpha = rep(alphaRange[i], length(bestRect[,1]))
    prob = rep(1/length(bestRect[,1]), length(bestRect[,1]))
    bestRect = cbind(bestRect, alpha, prob) # add a column so we know what alpha generated each rectangle
    gridSearch = rbind(gridSearch,bestRect)
  }
  
  colnames(gridSearch) = c("x1", "y1", "x2", "y2", "alpha", "prob") # name
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

  rect1 = as.numeric(rect1)
  rect2 = as.numeric(rect2)
  
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
    prop = size/max(findSize(rect1), findSize(rect2)) # prop = proportion of overlap
  } else {
    prop = 0
  }
  return(prop)
}  

# Step 3: Make a function that runs through each participant and finds the alpha that best describes them. 

# simulate some trial data: 

#' Generate some unique "true" rectangles by randomly sampling from all possible rectangles without replacement 
#'
#' @param nRectangles number of unique rectangles you want to generate
#' @param borders matrix of the coordinates of all possible rectangles in the grid
#'
#' @return nRectangle X 4 matrix of rectangle coordinates
#'
#' @examples genTrueRects(nRectangles = 5, borders = makeBorders(0:10))
genTrueRects = function(nRectangles, borders, minSize = 3) {
  sizes = findSize(borders)
  bordersMinSize = borders[sizes >= minSize,]
  trueRects = bordersMinSize[sample(1:length(borders[,1]),nRectangles),]
  return(trueRects) # <-- size needs to be greater than 3 (while loop?)
}


#' Generate some observations 
#' 
#' Generate observations for the learner in rectangle world for different nPoints conditions and different true rectangles
#'
#' @param nPos Vector or single number indicating the number of positive evidence points. If you want different nPoints 
#' in different conditions, represent this as a vector with each number corresponding to a different nPoints condition. 
#' @param nNeg Number of negative evidence points
#' @param nTriangles Number of unique triangles for each nObs condition
#' @param trueRects Matrix of rectangle coordinates for the true rectangles you want generate the data from (can be generated from genTrueRects function)
#'
#' @return Matrix of points labelled by trial and number of observations condition
#'
#' @examples 
#' generateObs(nPos = c(1, 2, 3), nNeg = c(1, 2, 3), nTriangles = 5, genTrueRects(nRectangles = 5, borders = makeBorders(0:10)))
#' 
generateObs = function(nPos, nNeg, nTriangles, trueRects) {
  
  allObs = NULL
  
  for (j in 1:length(nPos)) {
    nPosLoop = nPos[j]
    nNegLoop = nNeg[j]
    
    for (i in 1:nTrials) {
      obsTrial = samplePosNeg(nPosLoop, nNegLoop, trueRect = c(trueRects[i, ]))
      rownames(obsTrial) = NULL
      triangle = rep(i, nPosLoop + nNegLoop)
      nObsCond = rep(j, nPosLoop + nNegLoop)
      obsTrial = cbind(obsTrial, triangle, nObsCond)
      allObs = rbind(allObs, obsTrial)
    }
  }
  
  return(allObs)
}

## Simulate participant 

simulatePar = function(obs,
                       borders,
                       prior = "uniform",
                       alpha = 1) {
  rect = pedLearner(borders, obs, prior = "uniform", alpha = alpha) # generate probability distribution of pedagogical learner for these observations
  orderedRect =  order(rect[, "posterior"], decreasing = TRUE) # order from highest to lowest probability
  bestRect = rect[rect[, "posterior"] == rect[orderedRect[1], "posterior"], 1:4] # filter rectangle(s) with highest probability
  parRect = bestRect[sample(1:length(bestRect[, 1]), 1), ] # randomly sample from those rectangles
}

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

