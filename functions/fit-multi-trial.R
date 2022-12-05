# 
# borders = makeBorders(0:10)
# rectSmall = c(2,2,4,4)
# rectLarge = c(1,1,9,9)
# 
# obs = generateObs(c(1,2,3), c(0,1,2), rectSmall)
# 
# 
# t1 = simulatePar(obs[obs[,"nObsCond"] == "1",], borders)
# t2 = simulatePar(obs[obs[,"nObsCond"] == "2",], borders, alpha = 0)
# t3 = simulatePar(obs[obs[,"nObsCond"] == "3",], borders)

simulate_MultiTrials = function (observations, alpha = "default (1)") {
  obs = observations
  nObsConds = as.numeric(unique(obs[, "nObsCond"])) # unique trials (different "number of observations" condition)
  
  # make sure the default alpha is a vector the size of nObsConds
  if (alpha == "default (1)") {
    alpha = rep(1, length(nObsConds))
  } else if (length(alpha) == 1) {
    alpha = rep(alpha, length(nObsConds))
  }
  
  responses = NULL
  # for each trial, generate a rectangle that someone would have produced with a given alpha
  for (i in nObsConds) {
    alphaTrial = alpha[i]
    partRect = simulatePar(obs[obs[, "nObsCond"] == i, ], borders, alpha = alphaTrial)
    trial = cbind(partRect, i)
    responses = rbind(responses, trial)
  }
  names(responses) =  c("x1", "y1", "x2", "y2", "nObsCond")
  return(responses)
}


gridSearch_MultiTrials = function(observaitions, borders){
  obs = observaitions
  nObsConds = unique(obs[, "nObsCond"]) # unique trials (different "number of observations" condition)
  gridAllTrials = NULL
  for (i in nObsConds) {
    grid = alphaGridSearch(borders, obs[obs[, "nObsCond"] == i, ])
    nObsCond = i
    grid = cbind(grid, nObsCond)
    gridAllTrials = rbind(gridAllTrials, grid)
  }
return(gridAllTrials)
}

fitAlpha_MultiTrials = function (gridSearch, partRectangles){
allOverlap = NULL
# Calculate the overlap:  
  # For each participant response/rectangle drawn (trial) 
  for (j in 1:length(partRectangles[,1])){
    overlap = NULL
    gridSearchTrial = gridSearch[gridSearch[,"nObsCond"] == j,1:4] # filter the grid search for the "j" trial
    # For each of the rectangles predicted in the grid search
    for (i in 1:length(gridSearchTrial[, 1])) {
      overlap[i] = rectOverlap(gridSearchTrial[i,], partRectangles[j,])
    }
    allOverlap = c(allOverlap, overlap)
  }
  # data frame with degree of overlap between each alpha prediction (from grid search) and the participant's rectangle
  overlapGrid = cbind(gridSearch, allOverlap)
  
  # calculate the mean degree of overlap for the rectangles predicted by each alpha
  # By using the mean overlap, this should account for the fact that there may be multiple rectangles
  # predicted by the same alpha, as it mean accounts for the number of rectangles there are. 
  fit = aggregate(allOverlap ~  alpha, data = overlapGrid, FUN = mean)
  bestAlpha = fit[fit[,"allOverlap"] == max(fit[,"allOverlap"]),]
  return(bestAlpha)

}
