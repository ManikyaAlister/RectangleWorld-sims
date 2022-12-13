alphaGridSearchDist = function(borders,
                               observations,
                               alphaRange = seq(from = -1, to = 1, by = .2),
                               prior = "uniform") {
  gridSearch = NULL
  for (i in 1:length(alphaRange)) {
    rect =  pedLearner(borders, observations, prior, alpha = alphaRange[i]) # generate all eligible rectangles (hypotheses)
    #orderedRect =  order(rect[, "posterior"], decreasing = TRUE) # order from highest to lowest probability
    #bestRect = rect[rect[, "posterior"] == rect[orderedRect[1], "posterior"], 1:4] # take the most probable rectangle, keeping only the coordinate information (columns 1:4)
    #posterior = rect[rect[, "posterior"] == rect[orderedRect[1], "posterior"], "posterior"] # probs a more efficient way to do this
    alpha = rep(alphaRange[i], length(rect[, 1]))
    #prob = rep(1 / length(bestRect[, 1]), length(bestRect[, 1])) # "prob" is necessary because in some scenarios a given alpha will say that multiple rectangles are most likely. For example,
    # when alpha = 0, all eligible rectangles are equally possible. Therefore, "prob" corresponds to 1/no. of unique "best" rectangles at that alpha level. This way even though if an alpha of 1
    # and an alpha of 0 both predict the same rectangle to be the best, we will say that it corresponds to alpha = 1 because there were fewer possible rectangles to match with.
    
    rectAlpha = cbind(rect, alpha) # add a column so we know what alpha generated each rectangle

    
    gridSearch = rbind(gridSearch, rectAlpha)
  }
  
  #colnames(gridSearch) = c("x1", "y1", "x2", "y2", "posterior", "alpha", "prob") # name
  colnames(rectAlpha) = c(colnames(rect), "alpha")
  return(gridSearch)
  
}
