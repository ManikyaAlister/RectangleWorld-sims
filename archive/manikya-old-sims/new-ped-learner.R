newPedLearner = function(borders, observations, alpha = 1, prior = "uniform", borderSize = 100) {
    
    # set up default prior if no other is specified 
    prior = ifelse(prior == "uniform", rep(1,length(borders[,1]))/sum(rep(1,length(borders[,1]))), prior)
    
    # Rule out ineligible rectangles ----------------------------------------------------------------------
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
      hasNegEvidence = if (length(as.data.frame(negInRect)) > 1) {
        hasNegEvidence = rowSums(negInRect) > 0
      } else {
        hasNegEvidence = negInRect
      }
      
    }
    
    hypotheses = cbind(borders, hasPosEvidence, hasNegEvidence)
    hypotheses = hypotheses[hypotheses[, "hasPosEvidence"] == TRUE &
                              hypotheses[, "hasNegEvidence"] == FALSE, ]
    
    # Pedagogical likelihood --------------------------------------------------------------------------------------------
    
    # Calculate the number of positive and negative observations
    if (is.vector(observations)){
      numPos = sum(observations["category"]=="positive")
      numNeg = sum(observations["category"]=="negative")
    } else {
      numPos = sum(observations[,"category"]=="positive")
      numNeg = sum(observations[,"category"]=="negative")
    }
    
    
    # calculate the area outside of each prospective rectangle (hypothesis)
    sizeNeg = findSizeNeg(hypotheses, borderSize = borderSize) 
    
    # calculate the area inside of each prospective rectangle (hypothesis)
    sizePos = findSize(hypotheses)
    
    s = max(sizePos) # largest eligible hypothesis (following logic that teacher would have chosen negative points that were closer to the poisitive points if the rectangle was smaller)
    
    if (numNeg == 0) {
      likelihood = 1/(sizePos^(numPos))
    } else {
      likelihood = 1/(s/sizePos)^(sizePos/(numPos+numNeg))
    }
    
    posterior = (likelihood*prior)/sum(likelihood*prior)
    
    logLikelihood = log(likelihood)
    
    pedHypotheses = cbind(hypotheses[,1:4], sizePos,sizeNeg,prior,likelihood, logLikelihood,posterior)
    
    colnames(pedHypotheses) = c( # <-- rename cols
      "x1",
      "y1",
      "x2",
      "y2",
      "sizePos",
      "sizeNeg",
      "prior",
      "likelihood",
      "logLikelihood",
      "posterior"
    )
    return(pedHypotheses)
  
}
