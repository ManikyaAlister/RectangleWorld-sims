getLearnerPointDistribution = function(){
  # Source functions
  source(here("genericFunctions.R"))
  source(here("calculatingFunctions.R"))
  source(here("plottingFunctions.R"))
  
  # Load the pre-calculated data if not loaded already 
  if(!exists("hyp")){
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn)) 
  }
  
  # Recursive learner
  if (recursion == TRUE) {
    fileSeg <- paste0("x0to", H, "y0to", H)
    # learner assumes teacher is helpful (and teacher knows)
    if (alpha > 0 & alpha < 1) {
      fn <- paste0("datafiles/", fileSeg, "recursiveLow.RData")
      load(here(fn))
      recursionLevel <- paste0("H", str_replace(alpha, "0.", "0"))
    } else if (alpha == 1) {
      fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
      load(here(fn))
      recursionLevel <- paste0("H", alpha)
    } else if (alpha >= 2) {
      fn <- paste0("datafiles/", fileSeg, "recursiveHigh.RData")
      load(here(fn))
      recursionLevel <- paste0("H", alpha)
      # learner assumes teacher is weak/random (and teacher knows)
    } else if (alpha == 0) {
      fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
      load(here(fn))
      recursionLevel <- "W"
    } else if (alpha == -1) {
      fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
      load(here(fn))
      recursionLevel <- paste0("D", abs(alpha))
      # learner assumes teacher is deceptive (and teacher knows)
    } else if (alpha < 0 & alpha > -1) {
      fn <- paste0("datafiles/", fileSeg, "recursiveLow.RData")
      load(here(fn))
      recursionLevel <- paste0("D", str_replace(alpha, "-0.", "0"))
    } else if (alpha <= -2) {
      fn <- paste0("datafiles/", fileSeg, "recursiveHigh.RData")
      load(here(fn))
      recursionLevel <- paste0("D", abs(alpha))
    }
    # rename recursive all prob points array so it is generic
    recursionFile <- paste0(recursionLevel, "allProbPts")
    allProbPts <- get(recursionFile)
  }
  
  
  
  # Create indexing column for points (necessary for updating hypotheses)
  pts$index = 1:length(pts[, 1])
  
  #  Set scenario parameters  ---------------------------------------------
  
  
  # All hypotheses tracked by the learner
  lnHyp <- hyp 
  
  # All points tracked by the learner
  lnPts <- pts
  
  # set initial prior over hypotheses
  if (prior == "normal") {
    lnHyp$prior <- normalPrior(hyp$size)
  }
  
  # prior is just the posterior from the last trial
  lnHyp$posterior <- lnHyp$prior
  
  # Create an index column that keeps track of each hypothesis, so it's easier to compare them
  lnPts$index <- 1:length(lnPts[, 1])
  lnHyp$index <- 1:length(lnHyp[, 1])
  
  
  
  # set alphas based on function input
  lA <- which(alphas == alpha)
  lnAlphaText <- returnAlpha(alpha)
  
  # Set up empty data structured to be filled each trial
  learnerHypDist = list()
  learnerPtDist = list()
  obs = NULL
  
  for (i in 1:nTrials) {
    # Teacher samples new point
    newPt <- observations[i,]
    # Find the index of that point in the pre-calculated points (necessary for probability functions)
    newPt$index <-
      pts[newPt[, 1] == pts[, 1] & newPt[, 2] == pts[, 2], "index"]
    # Combine this new point with any points that have been sampled previously
    obs <- rbind(obs, newPt)
    obs <- obs %>% filter(!is.na(x))
    # learner updates their estimate of the hypotheses, given the point that was generated
    lnHyp <-
      updateHypotheses(allProbPts[, , lA], consPts, newPt, lnPts)
    
    # learner updates their estimate of the point distribution,
    lnPts <-
      updatePoints(allProbPts[, , lA], consPts, newPt, lnPts)
    
    # Remove impossible hypotheses
    lnPtsClean <- lnPts[lnPts[,"posterior"] > 0,]
    
    # Select relevant information
    lnPtsClean <- lnPtsClean[,c("index", "x1", "y1", "x2", "y2", "prior", "posterior", "size")]
    
    lnPtsClean <- cbind(lnPtsClean, alpha, clue = i)
    
    sizeOrder <- order(lnPtsClean[,"size"])
    
    lnPtsClean <- lnPtsClean[sizeOrder,]
    
    # Record the learner distribution over hypotheses for this trial
    learnerHypDist[[i]] <- lnPtsClean
  }
}