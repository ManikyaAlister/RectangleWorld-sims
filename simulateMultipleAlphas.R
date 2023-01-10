library(here)
library(ggpubr)
source(here("genericFunctions.R"))
source(here("calculatingFunctions.R"))
source(here("plottingFunctions.R"))

# Function to compare multiple learners on the same set of observations. 
multiAlphaPredictions = function(observations, allAlphas = c(-1,0,1),H = 10, save = FALSE,   prior = "normal"){

  # The number of different alpha predictions to test
  nAlphas <- length(allAlphas)
  
  # Number of best hypotheses to plot 
  nBestH <- 1
  
  # Load the pre-calculated data
  fileSeg <- paste0("x0to", H, "y0to", H)
  fn <- paste0("datafiles/", fileSeg, ".RData")
  load(here(fn))
  pts$index <- 1:length(pts[,1])
  
  # Number of trials in each block (one new observation each trial)
  nTrials <- length(observations[,1])
  
  # Empty data frame to track all of the points that have been sampled. 
  ptsSoFar <- NULL
  
  # Empty list to fill with each plot within a trial. 
  allPlots <- list()
  
  # Empty list to fil with the hypotheses of each learner
  lnHyps <- list()
  # Instantiate the learner's hypotheses for each learner alpha
  for(alpha in 1:nAlphas) {
    lnHyps[[alpha]] <- hyp
    ## Flat prior unless normal is specified. 
    if (prior == "normal") {
      lnHyps[[alpha]]$prior <- normalPrior(hyp$size, 50, 15)
    }
    # Prior is always the posterior of the previous iteration, so on the first iteration the posterior is the prior
    lnHyps[[alpha]]$posterior <- lnHyps[[alpha]]$prior
    
  }
  
  facetData <- NULL
  
  # Loop through each trial in a block (one new observation each trial)
  for (j in 1:nTrials) {
    # Empty data frame to fill with observations and rectangle predictions for this trial.  
    plotData <- NULL
    # Get the new point for this trial 
    newPt <- observations[j, ]
    # Get the index of this point, so that we know whether it is positive or negative
    newPt$index <-
      pts[newPt[, 1] == pts[, 1] & newPt[, 2] == pts[, 2], "index"]
    # Keep track of the points that have been sampled in previous trials
    ptsSoFar <- rbind(ptsSoFar,newPt)
    # Get the predictions of each learner alpha for this trial
    
    # Empty array to fill with data from a given trial for facet plot 
    facetDataTrial <- NULL
    
    for (i in 1:length(allAlphas)) {
      # Set the learner alpha for this iteration
      lnAlpha <- allAlphas[i]
      lA <- which(alphas == lnAlpha)
      
      # Empty array to fill with data for a given alpha within a given trial for facet plot 
      alphaData <- NULL
      
       # Get the learner's hypotheses for this alpha
      lnHyp <- lnHyps[[i]]

      # Update learner hypotheses with new point  
      lnHyp <-
        updateHypotheses(allProbPts[, , lA], consPts, newPt, lnHyp)
      bestH <- returnBestHypothesis(lnHyp, n = nBestH)
      bestRect <- lnHyp[bestH, ]
      
      # Fill relevant alpha data for facet plot 
      alphaData <- cbind(ptsSoFar, lnAlpha, bestRect[,1:4], j)
      # Fill relevant trial data for facet plot 
      facetDataTrial <- rbind(facetDataTrial, alphaData)

      # Get the best hypothesis for this alpha in this trial
      bestHAlpha <- cbind(bestRect, lnAlpha, j)
      # Update and store plot data for next trial
      plotData <- rbind(plotData, bestHAlpha)
      # Update and store the learner hypothesis for this trial for the next iteration
      lnHyps[[i]] <- lnHyp
    }
    plotData$lnAlpha <- factor(allAlphas)
    ptsSoFar$category <- as.factor(ptsSoFar$category)
    
    facetData <- rbind(facetData, facetDataTrial)
    
    p <- plotData %>%
      ggplot() +
      xlim(c(0,10))+
      ylim(c(0,10))+
      geom_rect(
        aes(
          xmin = x1,
          xmax = x2,
          ymin = y1,
          ymax = y2,
          fill = lnAlpha,
          colour = lnAlpha
        ),
        alpha = 0.2
      ) +
      geom_point(data = ptsSoFar, aes(x = x, y = y, shape = category))
    allPlots[[j]] <- p
  }
  
  plotAll <- ggarrange(plotlist = allPlots)    
  
  # UP TO HERE 
   facetPlot <- facetData %>%
    mutate(catagory = as.factor(category),
           lnAlpha = as.factor(lnAlpha))%>%
    ggplot() +
    xlim(c(0,10))+
    ylim(c(0,10))+
    geom_rect(
      aes(
        xmin = x1,
        xmax = x2,
        ymin = y1,
        ymax = y2,
        fill = lnAlpha
        #colour = lnAlpha
      ),
      alpha = 0.3
    ) +
    geom_point(aes(x = x, y = y, colour = category), size = 4)+
    #scale_fill_manual(values = c("lightblue","lightyellow"))+
    scale_color_manual(values = c("red","darkgreen"))+
    facet_wrap(~j)
    
  
  if (save == TRUE) {
    xPointsStr <- paste0(observations$x+0.5, collapse = "")
    yPointsStr <- paste0(observations$y+0.5, collapse = "")
    scenarioCode <- paste0("target-x-",xPointsStr,"-y-", yPointsStr)
    save(observations, file = here(paste0("experiment-scenarios/target-blocks/data/",scenarioCode,".Rdata")))
    ggsave(facetPlot, file = here(paste0("experiment-scenarios/target-blocks/figures/",scenarioCode,".png")), width = 10, height = 10)
  }
  
  facetPlot
  
}
