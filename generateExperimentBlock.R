##############################

# Generate Experiment Block #

#############################
library(here)
library(ggpubr)

#' Generate an experiment block for the rectangle game (teacher observations from a true rectangle)
#'
#' @param trueRectSize Size of true rectangle to be generated (small, medium or large)
#' @param tchAlpha Alpha of the teacher generating points
#' @param tchLnAlpha Alpha that the teacher thinks the learner thinks the teacher has 
#' @param lnAlpha Alpha the learner actually thinks the teacher has
#' @param nTrials Number of trials within a block (one observation per trial)
#' @param nBestH Number of best hypotheses to be plotted
#' @param maximise When maximise = TRUE, the teacher chooses the best point, rather than sampling from the distribution
#' @param H Maximum number size of the grid axes. 
#' @param rect Vectors of coordinates for the true rectangle the teacher is trying to communicate. When "random", a random rectangle is generated of size "trueRectSize". 
#' @param prior Prior probability of each rectangle hypothesis. 
#' @param scenarioCode Unique scenario code that will be used to save data pertaining to the particular run. 
#'
#' @return Plot showing the true rectangle and the point the teacher has chosen for each trial of the block. 
#'
#' @examples createExperimentBlock(tchAlpha = 1)
createExperimentBlock = function(trueRectSize = "small",
                                 tchAlpha,
                                 tchLnAlpha = "same",
                                 lnAlpha = 1,
                                 nTrials = 4,
                                 nBestH = 3,
                                 maximise = TRUE,
                                 H = 10,
                                 rect = "random",
                                 prior = "normal",
                                 scenarioCode = "exploritory") {
  # Set up   --------------------------------------------------------
  
  # Source functions
  source(here("genericFunctions.R"))
  source(here("calculatingFunctions.R"))
  source(here("plottingFunctions.R"))
  
  # Load the pre-calculated data
  fileSeg <- paste0("x0to", H, "y0to", H)
  fn <- paste0("datafiles/", fileSeg, ".RData")
  load(here(fn))
  
  #  Set scenario parameters  ---------------------------------------------
  # All points tracked by the teacher
  tchPts <- pts
  # All hypotheses tracked by the teacher
  tchHyp <- hyp
  # All hypotheses tracked by the learner
  lnHyp <- hyp 
  # Number of trials in a block/points provided by the teacher
  nTrials <- 4
  # Draw the index of a rectangle from all possible rectangles.
  trueHNum <- createRectangle(hyp, trueRectSize)
  # Coordinates of rectangle
  trueH <- getCoordinates(hyp, trueHNum)
  # Alpha the teacher thinks the learner thinks the teacher has
  tchLnAlpha <- ifelse(tchLnAlpha == "same", tchAlpha, tchLnAlpha)
  
  # In case user wants to test a custom rectangle
  if (!is.character(rect)) {
    trueH <- rect
    trueHNum <- rownames(hyp[hyp[,1] == rect[1] & hyp[,2] == rect[2] & hyp[,3] == rect[3] & hyp[,4] == rect[4],])
    trueRectSize <- "custom"
  }
  
  # Empty vector to fill with all of the chosen observations in a block
  obs <- NA
  # Empty list to fill with the observations at each trial within the block.
  trialObs <- list()
  
  # set alphas based on parameters above
  tA <- which(alphas == tchAlpha)
  tchAlphaText <- returnAlpha(tchAlpha)
  tlA <- which(alphas == tchLnAlpha)
  tchLnAlphaText <- returnAlpha(tchLnAlpha)
  lA <- which(alphas==lnAlpha)
  lnAlphaText <- returnAlpha(lnAlpha)
  # Configure file code with block parameters -------------------------------
  
  if (scenarioCode == "explore-scenarios"){
    scenarioCode <-
      paste0(trueRectSize, "-", trueHNum, "-tch-alpha-", tchAlpha, "tch-ln-alpha-",tchLnAlpha,"-prior-",prior)
    directory <- "explore-scenarios"
  } else {
    scenarioCode <- scenarioCode
    directory <- "chosen-scenarios"
  }

  
  
  # Generate points by the teacher ------------------------------------------
  
  # set initial prior: prior is just the posterior from the last prior
  if (prior == "normal"){
    tchHyp$prior <- normalPrior(hyp$size)
  }
  
  tchHyp$posterior <- tchHyp$prior
  
  # empty vecotr to fill with the names of plots
  plots <- NULL
  for (i in 1:nTrials) {
    # first step: the teacher generates a sampling distribution over points
    tchPts$posterior <-
      getSamplingDistribution(
        allProbPts[, , tlA],
        consPts,
        tchPts,
        trueHNum,
        priors = tchHyp$posterior,
        alpha = tchAlpha,
        obs = obs
      )
    t <- paste0("Teacher sampling distribution: alpha=", tchAlpha)
    
    # step two: teacher samples the next point based on that distribution
    newPt <-
      sampleNextPoint(
        consPts,
        pts,
        trueHNum,
        abs(tchPts$posterior),
        obs = obs,
        maximise = maximise
      )
    ## Keep track of which points have been sampled
    obs <- rbind(obs, newPt)
    obs <- obs %>% filter(!is.na(x))
    
    ## Keep a list of the observations in each block, so it's easier to model each trial within the block
    trialObs[[i]] <- obs
    
    ## Update data frame of all possible to indicate that this point has been chosen.
    tchPts[as.character(newPt["name"]), "selected"] = TRUE
    
  
    ## Create json data corresponding to the coded experiment grid structure describing all possible points and whether they were observed.
    trialJson <- tchPts %>%
      mutate(col = x + 0.5,
             row = y + 0.5,
             observed = "none") %>%
      select(row, col, observed)
    
    trialJson[obs[, "name"], "observed"] <- obs[, "category"]

    
    # step three: teacher updates their estimate of the learner's distribution over hypotheses, given the point that was generated
    tchHyp <- updateHypotheses(allProbPts[, , tlA], consPts, newPt, tchHyp)
    t <- "Teacher's chosen points + hypothesis distribution for learner"
    st <-
      paste0("Teacher's alpha = ", tchAlpha, "True rect no. = ", trueHNum)
    
    # step four: learner updates their estimate of the hypotheses, given the point that was generated
    lnHyp <- updateHypotheses(allProbPts[,,lA],consPts,newPt,lnHyp)
    bestH <- returnBestHypothesis(lnHyp,n=nBestH)
    t <- "Learner's actual hypothesis distribution"
    pt1step5 <- plotHypotheses(trueH,obs,lnHyp[bestH,],xrange=xrange,
                               yrange=yrange,title=t,subtitle=st)
    
    
    plot <- plotHypotheses(
      trueH,
      obs,
      lnHyp[bestH, ],
      xrange = xrange,
      yrange = yrange,
      title = t,
      subtitle = st
    )
    
    plots[[i]] <- assign(paste0("pt", i, "plot"), plot)
    
  }
  
  
  
  #plots <- c(pt1step3, pt2step3,pt3step3,pt4step3)
  blockScenario <- ggarrange(plotlist = plots)
  
  # Save plots and data -----------------------------------------------------
  
  # plots
  ggsave(
    blockScenario,
    filename = here(
      paste0(
        "experiment-scenarios/",directory,"/figures/",
        scenarioCode,
        ".png"
      )
    ),
    height = 10,
    width = 10
  )
  
  # data
  
  ## Make a new directory for all the data within a given scenario
 dir.create(here(paste0("experiment-scenarios/",directory,"/data/",scenarioCode,"/")))
  
  ## vector of all observations in a block
  save(obs, file = here(
    paste0(
      "experiment-scenarios/",directory,"/data/",scenarioCode,"/",
      scenarioCode,
      "-obs.Rdata"
    )
  ))
  ## List with all observations in each trial within a block
  save(trialObs, file = here(
    paste0(
      "experiment-scenarios/",directory,"/data/",scenarioCode,"/",
      scenarioCode,
      "trial-obs.Rdata"
    )
  ))
  
  ## json data
  save(trialJson, file = here(
    paste0(
      "experiment-scenarios/",directory,"/data/",scenarioCode,"/",
      scenarioCode,
      "-trialJson-t",
      i,
      ".Rdata"
    )
  ))
  
  # return
  blockScenario
}

