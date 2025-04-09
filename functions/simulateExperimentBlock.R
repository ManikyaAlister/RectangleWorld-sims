


simulateExperimentBlock = function(observations,
                                   trueH,
                                   output = "distribution-plot",
                                   nTrials = 4,
                                   learnerAlpha = 1,
                                   H = 10,
                                   nBestH = 3,
                                   prior = "normal") {
  # Source functions
  source(here("functions/genericFunctions.R"))
  source(here("functions/calculatingFunctions.R"))
  source(here("functions/plottingFunctions.R"))
  
  # Load the pre-calculated data
  fileSeg <- paste0("x0to", H, "y0to", H)
  fn <- paste0("datafiles/", fileSeg, ".RData")
  load(here(fn))
  
  # Create indexing column for points (necessary for updating hypotheses)
  pts$index = 1:length(pts[, 1])
  
  #  Set scenario parameters  ---------------------------------------------
  
  
  # All hypotheses tracked by the learner
  lnHyp <- hyp
  
  # set prior
  #lnHyp$prior <-
  
  # All points tracked by the learner
  lnPts <- pts
  
  # set initial prior over hypotheses
  if (prior == "normal") {
    lnHyp$prior <- normalPrior(hyp$size)
  }
  
  # prior is just the posterior from the last trial
  lnHyp$posterior <- lnHyp$prior
  
  
  # set alphas based on function input
  lA <- which(alphas == learnerAlpha)
  lnAlphaText <- returnAlpha(learnerAlpha)
  
  # Set up empty data structured to be filled each trial
  learnerHypDistributins = list()
  allHypPlots = list()
  allDistPlots = list()
  obs = NULL
  
  # Loop over each trial in the block
  for (i in 1:nTrials) {
    # Step 1: teacher samples new point
    newPt <- observations[i, ]
    newPt$index <-
      pts[newPt[, 1] == pts[, 1] & newPt[, 2] == pts[, 2], "index"]
    ## Combine this new point with any points that have been sampled previously
    obs <- rbind(obs, newPt)
    obs <- obs %>% filter(!is.na(x))
    # Step 2
    # step five: learner updates their estimate of the hypotheses, given the point that was generated
    lnHyp <- updateHypotheses(allProbPts[, , lA], consPts, newPt, lnHyp)
    
    lnPts <- updatePoints(posProbPts[, , lA],
                          newPt,
                          posterior = lnHyp$posterior,
                          pts = lnPts)
    
    bestH <- returnBestHypothesis(lnHyp, n = nBestH)
    
    st <- paste0("alpha = ", learnerAlpha)
    t <- "Learner's actual hypothesis distribution"
    hypPlot <- plotHypotheses(
      trueH,
      obs,
      lnHyp[bestH, ],
      xrange = xrange,
      yrange = yrange,
      title = t,
      subtitle = st
    )
    t <- "Learner's actual point distribution"
    distPlot <-
      plotDistribution(
        allPts = lnPts,
        xrange = xrange,
        yrange = yrange,
        trueRectangle = trueH,
        obs = obs,
        whichDist = "posterior",
        title = t,
        subtitle = st
      )
    
    learnerHypDistributins[[i]] = lnHyp
    allHypPlots[[i]] <- hypPlot
    allDistPlots[[i]] <- distPlot
  }
  
  
  if (output == "distribution-data") {
    learnerHypDistributins
  } else if (output == "distribution-plot") {
    ggarrange(plotlist = allDistPlots)
  } else if (output == "best-hyp-plot") {
    ggarrange(plotlist = allHypPlots)
  }
  
  
  
}
