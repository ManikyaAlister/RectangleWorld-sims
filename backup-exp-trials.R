

##############################

# Generate Experiment Block #

#############################
rm(list = ls())
library(here)
library(ggpubr)



# Configure block parameters ----------------------------------------------

# Set X and Y range for rectangle grid
H <- 10
# Size of the true rectangle
trueRectSize <- "medium"
# Teacher's alpha
tchAlpha <- 1
# Alpha the teacher thinks the learner thinks the teacher has
tchLnAlpha <- 1
# Number of best hypotheses plotted
nBestH <- 3
# Configure whether the teacher is choosing the best point or sampling proportional to distribution
maximise <- TRUE


createExperimentBlock = function(trueRectSize = "small",
                                 tchAlpha,
                                 tchLnAlpha = "same",
                                 nTrials = 4,
                                 nBestH = 3,
                                 maximise = TRUE,
                                 H = 10) {
  # Set up   --------------------------------------------------------
  
  # Source functions
  source(here("genericFunctions.R"))
  source(here("calculatingFunctions.R"))
  source(here("plottingFunctions.R"))
  
  # Load the pre-calculated data
  fileSeg <- paste0("x0to", H, "y0to", H)
  fn <- paste0("datafiles/", fileSeg, ".RData")
  load(here(fn))
  
  #  Set block parameters  ---------------------------------------------
  # All points tracked by the teacher
  tchPts <- pts
  # All hypotheses tracked by the teacher
  tchHyp <- hyp
  # Number of trials in a block/points provided by the teacher
  nTrials <- 4
  # Draw the index of a rectangle from all possible rectangles.
  trueHNum <- createRectangle(hyp, trueRectSize)
  # Coordinates of rectangle
  trueH <- getCoordinates(hyp, trueHNum)
  # Alpha the teacher thinks the learner thinks the teacher has
  tchLnAlpha <- ifelse(tchLnAlpha == "same", tchAlpha, tchLnAlpha)
  
  # Empty vector to fill with all of the chosen observations in a block
  obs <- NA
  # Empty list to fill with the observations at each trial within the block.
  trialObs <- list()
  
  # set alphas based on parameters above
  tA <- which(alphas == tchAlpha)
  tchAlphaText <- returnAlpha(tchAlpha)
  tlA <- which(alphas == tchLnAlpha)
  tchLnAlphaText <- returnAlpha(tchLnAlpha)
  
  # Make file code with block parameters -------------------------------
  scenarioCode <-
    paste0(trueRectSize, "-", trueHNum, "-tch-alpha-", tchAlpha)
  
  # Generate points by the teacher ------------------------------------------
  
  # set initial prior: prior is just the posterior from the last prior
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
        alpha = tchAlpha
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
    save(trialJson, file = here(
      paste0(
        "experiment-scenarios/teacher-points/data/",
        scenarioCode,
        "-trialJson-t",
        i,
        ".Rdata"
      )
    ))
    
    # step three: teacher updates their estimate of the learner's distribution over hypotheses, given the point that was generated
    tchHyp <- updateHypotheses(allProbPts[, , tlA], consPts, newPt, tchHyp)
    bestH <- returnBestHypothesis(tchHyp, n = nBestH)
    t <- "Teacher's chosen points + hypothesis distribution for learner"
    st <-
      paste0("Teacher's alpha = ", tchAlpha, "True rect no. = ", trueHNum)
    step3 <- plotHypotheses(
      trueH,
      obs,
      tchHyp[bestH, ],
      xrange = xrange,
      yrange = yrange,
      title = t,
      subtitle = st
    )
    
    plots[[i]] <- assign(paste0("pt", i, "step3"), step3)
    
  }
  
  
  
  #plots <- c(pt1step3, pt2step3,pt3step3,pt4step3)
  blockScenario <- ggarrange(plotlist = plots)
  
  # Save plots and data -----------------------------------------------------
  
  # plots
  ggsave(
    blockScenario,
    filename = here(
      paste0(
        "experiment-scenarios/teacher-points/figures/",
        scenarioCode,
        ".png"
      )
    ),
    height = 10,
    width = 10
  )
  
  # data
  ## vector of all observations in a block
  save(obs, file = here(
    paste0(
      "experiment-scenarios/teacher-points/data/",
      scenarioCode,
      "-obs.Rdata"
    )
  ))
  ## List with all observations in each trial within a block
  save(trialObs, file = here(
    paste0(
      "experiment-scenarios/teacher-points/data/",
      scenarioCode,
      "trial-obs.Rdata"
    )
  ))
  
  # return
  blockScenario
}

test <- createExperimentBlock(trueRectSize = "large", tchAlpha = 1)
