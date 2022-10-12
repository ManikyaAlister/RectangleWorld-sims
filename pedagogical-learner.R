### Pedagogical Learner ###
library(here)


isInRectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}


#' simulates a learner who assumes observations are sampled pedagogically
#'  
#' @param alpha Numeric value from -inf to inf. determines how helpful the learner thinks the teacher is. Alpha = 0 is equivalent to weak sampling, alpha = 1 is standard pedagogical sampling. 
#' @param borders Array of hypothesised category boundary points (coordinates of rectangles)
#' @param observations Points that have been labelled as belonging to a certain category
pedLearner = function(borders, observations, alpha = 1) {
  posInRect = areInCat(borders, observations, "positive")
  if (is.na(posInRect[1])) {
    hasPosEvidence = rep(TRUE,length(borders[,1])) # if there is no positive evidence, then all of the hypotheses are valid for this condition
  } else { f = function(isInRect) setequal(isInRect, rep(TRUE, length(as.data.frame(isInRect)))) # make isInRect a function suitable for apply
  # Find out which rectangles contain positive evidence
  hasPosEvidence = if (length(as.data.frame(posInRect)) > 1) {     
    apply(posInRect, 1, f)
  } else {
    # if length is 1, no need to do the above function
    hasPosEvidence = posInRect
  }
    
  }
  
  negInRect = areInCat(borders, observations, "negative") # does the hypothesised rectangle contain negative evidence?
  if (is.na(negInRect[1])) {
    hasNegEvidence =  rep(FALSE,length(borders[,1]))
  } else {  
    hasNegEvidence = if (length(as.data.frame(posInRect)) > 1) {
    hasNegEvidence = rowSums(negInRect) > 0
  } else {
    hasNegEvidence = negInRect
  }
    
  }
  
  hypotheses = cbind(borders, hasPosEvidence, hasNegEvidence)
  hypotheses = hypotheses[hypotheses[, "hasPosEvidence"] == TRUE &
                            hypotheses[, "hasNegEvidence"] == FALSE, ]
  
### pedagogical likelihood

  if (is.vector(observations)){
    numPos = sum(observations["category"]=="positive")
    numNeg = sum(observations["category"]=="negative")
  } else {
    numPos = sum(observations[,"category"]=="positive")
    numNeg = sum(observations[,"category"]=="negative")
  }
  
  sizeNeg = findSizeNeg(hypotheses)
  sizePos = findSize(hypotheses)
  
  likelihoodNeg = (1 / findSizeNeg(hypotheses) ^ numNeg) 
  likelihoodPos = (1 / findSize(hypotheses) ^ numPos) # non-log probability so that I can integrate it with alpha/transparency when plotting
  likelihood = (likelihoodNeg*likelihoodPos)^alpha
  
  posterior = likelihood/sum(likelihood)
  
  logLikelihood = log(likelihood)
  
  pedHypotheses = cbind(hypotheses[,1:6], sizePos,sizeNeg,likelihood, likelihoodPos, likelihoodNeg,logLikelihood,posterior)
  
  colnames(pedHypotheses) = c(
    "x1",
    "y1",
    "x2",
    "y2",
    "hasPosEvidence",
    "hasNegEvidence",
    "sizePos",
    "sizeNeg",
    "likelihood",
    "likelihoodPos",
    "likelihoodNeg",
    "logLikelihood",
    "posterior"
  )
  return(pedHypotheses)
}

plotPed = function(hypotheses, observations, categoryBoundary, range = 1:10,nHypotheses = "all"){
  
  if (nHypotheses == "all"){
    nHypotheses = length(hypotheses[,1])
  } else {
    nHypotheses = nHypotheses
  }
  
  hypotheses = hypotheses[order(hypotheses[,"posterior"], decreasing = TRUE),]
  hypotheses = hypotheses[1:nHypotheses,]
  hypotheses[,"posterior"] = hypotheses[,"posterior"]/sum(hypotheses[,"posterior"])
  
  if (is.vector(observations)){ # plotting syntax is slightlly different when there's only 1 observation (i.e., it's a vector)
    plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
    rect(hypotheses[,1],hypotheses[,2],hypotheses[,3],hypotheses[,4], col= rgb(0,0,1.0,alpha=hypotheses[,"posterior"]), lwd = 0.01) # making alpha equivalent to the likelihood to show strong sampling gradient 
    points(observations[1], observations[2]) # this part is different when observations is a vector
    rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "red", lwd = 2)
  } else{
    plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
    rect(hypotheses[,1],hypotheses[,2],hypotheses[,3],hypotheses[,4], col= rgb(0,0,1.0,alpha=hypotheses[,"posterior"]), lwd = 0.01) # making alpha equivalent to the likelihood to show strong sampling gradient 
    points(observations)
    rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "red", lwd = 2)
  }
  
}

