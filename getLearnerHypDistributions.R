
#'  Calculate the probability distribution of different hypotheses/rectangles given a set of clues/observations and a given alpha
#'
#' @param observations A data frame of observations/clues provided by the teacher. Columns are "x": numeric x coordinates, "y": 
#' numeric y coordinates, "category": character indicating either "positive" evidence (inside rectangle) or "negative" evidence
#'(outside of rectangle)
#' @param H Size of hypothesis space (default is 10)
#' @param prior Whether the prior is "normal" (normally distributed over size, M = 50, SD =15; default), or "flat". 
#' @param alpha Alpha of the learner. How helpful the learner assumes the teacher is being. Default is 1. 
#' @param nTrials Number of trials to to iterate over, assuming one observation is shown each trial. 
#'
#' @return Data frame containing columns for: index of each rectangle, coordinates of each rectangle, prior, and posterior. 
#'
#' @examples
getLearnerHypDistribution = function(observations,
                                     H = 10,
                                     prior = "normal",
                                     alpha = 1,
                                     nTrials = 4) {
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
  
  
  # Create indexing column for points (necessary for updating hypotheses)
  pts$index = 1:length(pts[, 1])
  
  #  Set scenario parameters  ---------------------------------------------
  
  
  # All hypotheses tracked by the learner
  lnHyp <- hyp
  
  
  # All points tracked by the learner
  #lnPts <- pts
  
  # set initial prior over hypotheses
  if (prior == "normal") {
    lnHyp$prior <- normalPrior(hyp$size)
  }
  
  # prior is just the posterior from the last trial
  lnHyp$posterior <- lnHyp$prior
  
  # Create an index column that keeps track of each hypothesis, so it's easier to compare them
  lnHyp$index <- 1:length(lnHyp[, 1])
  
  
  # set alphas based on function input
  lA <- which(alphas == alpha)
  lnAlphaText <- returnAlpha(alpha)
  
  # Set up empty data structured to be filled each trial
  learnerHypDist = list()
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
      updateHypotheses(allProbPts[, , lA], consPts, newPt, lnHyp)
    
    # Remove impossible hypotheses
    lnHypClean <- lnHyp[lnHyp[,"posterior"] > 0,]
    
    # Select relevant information
    lnHypClean <- lnHypClean[,c("index", "x1", "y1", "x2", "y2", "prior", "posterior", "size")]
    
    # Record the learner distribution over hypotheses for this trial
    learnerHypDist[[i]] <- lnHypClean
  }
  
  learnerHypDist
}

# What is the probability of a learner drawing a particular rectangle under different alphas?

rectangleAlphaPosteriors = function(learnerRectangle, observations, nTrials = 1, alphasToSearch = "all-alphas", H = 10, prior = "normal"){
  # Load the pre-calculated data if not loaded already 
  if(!exists("hyp")){
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn)) 
  }
  
  # Create index column for all hypotheses
  hyp$index <- 1:length(hyp[,1])
  
  # Find out index of learner's rectangle
  hypIndex <- hyp[hyp[,"x1"] == learnerRectangle[1] & hyp[,"y1"] == learnerRectangle[2] & hyp[,"x2"] == learnerRectangle[3] & hyp[,"y2"] == learnerRectangle[4],"index"]
  
  # Check to see if we want to just search through all alphas that have already been predefined or a custom set of alphas
  if(is.character(alphasToSearch)){
    alphasToSearch <- alphas # alphas is loaded in the pre-calculated data
  } 
  
  alphaPosteriors = NULL
  
  # Get the probability of the learners drawn rectangle for each alpha  
  for(i in 1:length(alphasToSearch)){
    alpha =  alphasToSearch[i]
    dist <- getLearnerHypDistribution(observations, alpha = alpha, nTrials = nTrials, prior = prior)
    distUnlist <- dist[[1]]
    learnerRectangle <- distUnlist[distUnlist[,"index"]==hypIndex,]
    prob <- cbind(alpha, learnerRectangle[,"posterior"], hypIndex)
    alphaPosteriors <- rbind(alphaPosteriors,prob)
  }
  colnames(alphaPosteriors) = c("alpha", "posterior", "index")
  alphaPosteriors
  
}
