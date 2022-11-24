


samplePosNeg = function(nPos, nNeg, trueRect) {
  allPoints = expand.grid(seq(0.5, 9.5, 1), seq(0.5, 9.5, 1)) # set up a matrixx of all possibele points. Using intervals of .5 so points are inside the rectangles not on the borders.
  isPositive = isInRectangle(p = allPoints, r = trueRect) # check which of the points are inside the true rectangle and which aren't.
  evidence = cbind(allPoints, isPositive) # combine into a single matrix
  colnames(evidence) = c("x", "y", "isPositive") # assign column names
  
  # make a seperate data set for positive and negative evidene respectively
  pos = evidence[evidence[, "isPositive"] == TRUE, ] 
  neg = evidence[evidence[, "isPositive"] == FALSE, ]
  
  # sample from positive and negative evidence respectively 
  samplePos = sample(1:length(pos[, 1]), nPos) # sample a row number from positive/negative matrix respectively 
  sampleNeg = sample(1:length(neg[, 1]), nNeg)
  obsPos = pos[samplePos, ] # extract rows that have been sampled
  obsNeg = neg[sampleNeg, ]
  
  # other functions in this project need the observations label to be under a column labeled "category" with cells as either "postitive or "negative"
  
  categoryPos = rep("positive", nPos)
  categoryNeg = rep("negative", nNeg)
  category = c(categoryPos, categoryNeg)
  
  # combine coordinates and labels into a single matrix
  obs = rbind(obsPos[, 1:2], obsNeg[, 1:2])
  obs = cbind(obs, category)
  
  #return
  return(obs)
  
}


samplePosNegOld = function(nObsPos, nObsNeg, trueRectangle) {
  iNeg = 1
  iPos = 1
  
  obsPos = array(NA, dim = c(nObsPos, 3)) # set a really large number of rows so that there's enough to be filled
  obsNeg = array(NA, dim = c(nObsNeg, 3))
  
  
  while (iPos < nObsPos+1) {
    sample = weakSampler(1,trueRectangle)
    if (sample[,"category"] == "positive") {
      obsPos[iPos,] = sample
      iPos = iPos+1
    } 
  }
  
  while(iNeg < nObsNeg+1) {
    sample = weakSampler(1,trueRectangle)
    if (sample[,"category"] == "negative") {
      obsNeg[iNeg,] = sample
      iNeg = iNeg+1
    }
  }  
  
  
  obs = rbind(obsPos, obsNeg)
  colnames(obs) = c("x","y","category")
  return(obs)
  
}