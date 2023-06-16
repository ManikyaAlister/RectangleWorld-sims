library(here)
library(ggplot2)

#'  Calculate the probability distribution of different hypotheses/rectangles given a set of clues/observations and a given alpha
#'
#' @param observations A data frame of observations/clues provided by the teacher. Columns are "x": numeric x coordinates, "y": 
#' numeric y coordinates, "category": character indicating either "positive" evidence (inside rectangle) or "negative" evidence
#'(outside of rectangle)
#' @param H Size of hypothesis space (default is 10)
#' @param prior Whether the prior is "normal" (normally distributed over size, M = 50, SD =15; default), or "flat". 
#' @param alpha Alpha of the learner. How helpful the learner assumes the teacher is being. Default is 1. 
#' @param nTrials Number of trials to to iterate over, assuming one observation is shown each trial. 
#' @param recursion Whether the learner is thinking about what the teacher thinks they know. Currently coded so that the teachers alpha 
#' and the learner's alpha is always the same. The teacher's assumption about the learner for a given alpha needs to be pre-calculated in 
#' datafiles/ (currently only done for alpha = 1 (H), -1 (D), and 0 (W) 3/5/23)
#'
#' @return Data frame containing columns for: index of each rectangle, coordinates of each rectangle, prior, and posterior. 
#'
#' @examples
getLearnerHypDistribution = function(observations,
                                     H = 10,
                                     prior = "normal",
                                     alpha = 1,
                                     nTrials = 4,
                                     recursion = FALSE,
                                     allHyp = FALSE) {
  # Source functions
  source(here("genericFunctions.R"))
  source(here("calculatingFunctions.R"))
  source(here("plottingFunctions.R"))
  
  # Load the pre-calculated data if not loaded already 
  if(!exists("xrange")){
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
    
    # In some situations we might want to retain impossible hypotheses, in others not
    if (allHyp) {
      lnHypClean <- lnHyp
    } else {
      # Remove impossible hypotheses
      lnHypClean <- lnHyp[lnHyp[,"posterior"] > 0,]
      
      # Select relevant information
      lnHypClean <- lnHypClean[,c("index", "x1", "y1", "x2", "y2", "prior", "posterior", "size")]
      
      lnHypClean <- cbind(lnHypClean, alpha, clue = i)
      
      sizeOrder <- order(lnHypClean[,"size"])
      
      lnHypClean <- lnHypClean[sizeOrder,]
      
    }
    # Record the learner distribution over hypotheses for this trial
    learnerHypDist[[i]] <- lnHypClean

  }
  
  learnerHypDist
}

# What is the probability of a learner drawing a particular rectangle under different alphas?

rectangleAlphaPosteriors = function(learnerRectangle,
                                    observations,
                                    nTrials = 1,
                                    alphasToSearch = "all-alphas",
                                    H = 10,
                                    prior = "normal",
                                    recursion = FALSE) {
  # Load the pre-calculated data if not loaded already
  if (!exists("hyp")) {
    fileSeg <- paste0("x0to", H, "y0to", H)
    fn <- paste0("datafiles/", fileSeg, ".RData")
    load(here(fn))
  }
  
  # Create index column for all hypotheses
  hyp$index <- 1:length(hyp[, 1])
  
  # Find out index of learner's rectangle
  hypIndex <-
    hyp[hyp[, "x1"] == learnerRectangle[1] &
          hyp[, "y1"] == learnerRectangle[2] &
          hyp[, "x2"] == learnerRectangle[3] &
          hyp[, "y2"] == learnerRectangle[4], "index"]
  
  # Check to see if we want to just search through all alphas that have already been predefined or a custom set of alphas
  if (is.character(alphasToSearch)) {
    alphasToSearch <-
      alphas # alphas is loaded in the pre-calculated data
  }
  
  alphaPosteriors = NULL
  
  # Get the probability of the learners drawn rectangle for each alpha
  for (i in 1:length(alphasToSearch)) {
    alpha =  alphasToSearch[i]
    if (recursion){
      dist <-
        getLearnerHypDistribution(
          observations,
          alpha = alpha,
          nTrials = nTrials,
          prior = prior,
          recursion = TRUE
        )
    } else {
      dist <-
        getLearnerHypDistribution(
          observations,
          alpha = alpha,
          nTrials = nTrials,
          prior = prior
        )
    }
    
    distUnlist <- dist[[1]]
    learnerRectangle <- distUnlist[distUnlist[, "index"] == hypIndex, ]
    prob <- cbind(alpha, learnerRectangle[, "posterior"], hypIndex)
    alphaPosteriors <- rbind(alphaPosteriors, prob)
  }
  colnames(alphaPosteriors) = c("alpha", "posterior", "index")
  alphaPosteriors
  
}

# Get posterior probability of multiple learner guesses under different alphas 
#'
#' @param learnerRectangles Rectangles produced by participants. To simulate learner rectangles, see simulateLearnerGuesses()
#' @param observations Data frame of observations/clues indicating the true rectangle.
#' @param nTrials Number of trials to get the distributions for. 
#' @param alphasToSearch Which alphas to generate distributions for. Default is "all-alphas" which uses the pre-generated alphas. 
#' @param H Grid size. Default is 10
#' @param prior Prior distribution over hypotheses. The default is normally distributed over size. See normalPrior() in 
#' calculatingFunctions.R for specifics.
#'
#' @return A data frame with three columns: alpha, posterior, and index. Alpha is the alpha that generated a given posterior 
#' for a given rectangle (indicated by index)
#'
#' @examples
getMultiAlphaPosteriors = function(learnerRectangles,
                                   observations,
                                   nTrials = 1,
                                   alphasToSearch = "all-alphas",
                                   H = 10,
                                   prior = "normal",
                                   recursion = FALSE) {
  allPosteriors <- NULL
  for (i in 1:length(learnerRectangles[, 1])) {
    print(paste0((i/length(learnerRectangles[,1])*100),"%"))
    rect <- as.vector(as.matrix(learnerRectangles[i, c("x1","y1","x2","y2")]))
    if (recursion){
      posteriors <-
        rectangleAlphaPosteriors(
          learnerRectangle = rect,
          observations = observations,
          nTrials = nTrials,
          alphasToSearch = alphasToSearch,
          H = H,
          prior = prior,
          recursion = TRUE
        )
    } else {
      posteriors <-
        rectangleAlphaPosteriors(
          learnerRectangle = rect,
          observations = observations,
          nTrials = nTrials,
          alphasToSearch = alphasToSearch,
          H = H,
          prior = prior
        )
    }

    allPosteriors <- rbind(allPosteriors, posteriors)
  }
  
  allPosteriors <- as.data.frame(allPosteriors)
  allPosteriors
}


# Simulate learner guesses  -----------------------------------------------

# Sample rectangles at the probability predicted for a given alpha learner
#'
#' @param observations 
#' @param alpha Alpha of the learner generating the rectangles
#' @param trial 
#' @param nRectangles 
#' @param prior 
#'
#' @return
#' @export
#'
#' @examples
simulateLearnerGuesses = function(observations,
                                  alpha,
                                  trial,
                                  nRectangles,
                                  prior = "normal",
                                  recursion = FALSE) {
  # Get hypothesis (rectangle) distribution for a given set of clues and a given alpha
  if (recursion) {
    dist <-
      getLearnerHypDistribution(observations[1:trial, ],
                                alpha = alpha,
                                nTrials = length(1:trial),
                                prior = prior,
                                recursion = TRUE)
  } else {
    dist <-
      getLearnerHypDistribution(observations[1:trial, ],
                                alpha = alpha,
                                nTrials = length(1:trial),
                                prior = prior)
  }
  
  dist <- dist[[trial]]
  # Sample from rectangles with the probability corresponding to the actual probability of choosing that rectangle for alpha
  sampleIndexes <-
    sample(
      dist$index,
      size = nRectangles,
      prob = dist$posterior,
      replace = TRUE
    )
  # Get the coordinates of the rectangles
  sampleRects <- NULL
  for (i in 1:nRectangles) {
    index <- sampleIndexes[i]
    # Get the row of coordinates corresponding to the sampled index
    sampleCoords <-
      dist[dist[, "index"] == index, c("x1", "y1", "x2", "y2","size")]
    # Combine into data frame
    sampleRects <- rbind(sampleRects, sampleCoords)
  }
  # rename index column 
  index <- sampleIndexes
  #return
  sampleRects <- cbind(sampleRects, index)
}


#' Get the probability that learners of different alphas would have generated a given rectangle
#'
#' @param data Participant data filtered by block
#' @param block Experiment block that you are examining (e.g., target block 2 or 8)
#' @param alphas Vector of all of the alphas you are interested in fitting
#'
#' @return
#' @export
#'
#' @examples
fitAlphas = function(data, block, alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5), recursion = FALSE) {
  all_alpha_posteriors = NULL
  # loop through all alphas
  for (j in 1:length(alphas)) {
    alpha = alphas[j]
    all_posteriors = NULL
    # loop through each clue
    for (i in 1:length(data[, 1])) {
      block = data[i, "block"]
      clue = data[i, "clue"]
      cond = data[i, "cond"]
      # load pre-calculated probability distributions for the given experiment block and alpha. Make sure 
      # that the block has been pre-calculated or the code will break. 
            if (recursion == TRUE){
        load(here(
          paste0(
            "experiment-scenarios/hypothesis-distributions/b-",
            block,
            "-dist-alpha_",
            alpha,
            "-recursive-learner.Rdata"
          )
        ))  
      } else {
        load(here(
          paste0(
            "experiment-scenarios/hypothesis-distributions/b-",
            block,
            "-dist-alpha_",
            alpha,
            ".Rdata"
          )
        ))
      }
      
     
      # get the observations corresponding to the clue number
      resp_dist = dist[[clue]]
      # take the posterior of participant's rectangle
      posterior = resp_dist[resp_dist[, "index"] == data[i, "index"], ]
      posterior = select(posterior,-prior)
      # include whether respondent passed manipulation check
      man_check = data[i,"man_check"]
      posterior = cbind(posterior, cond, man_check)
      all_posteriors = rbind(all_posteriors, posterior)
    }
    all_alpha_posteriors = rbind(all_alpha_posteriors, all_posteriors)
  }
  all_alpha_posteriors
}
