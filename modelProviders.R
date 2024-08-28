library(here)
library(tidyverse)
library(ggpubr)

#' Get the point distributions given participant responses 
#'
#' @param H Maximum number size of the grid axes.
#' @param prior Prior probability of each rectangle hypothesis.
#' @param teacher_model Either helpful, misleading, or uninformative
#' @param teacher_data Data that has th teacher's actual chosen points. This should
#' only contain a single block of responses. 
#' @param print_plot When true, prints a plot showing the point distributions and the
#' actual points chosen by the participant on each trial. 
#'
#' @return List of the point distributions from each trial.
#'
#' @examples getPointDistributions(teacher_data)
getPointDistributions = function(
    teacher_data,
    teacher_model,
    H = 10,
    prior = "normal",
    print_plot = TRUE,
    save_plot = FALSE,
    subj_name = "",
    size = "",
    rank = FALSE) {
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
  nTrials <- length(teacher_data[, 1])
  # Draw the index of a rectangle from all possible rectangles.
  
  # Get true hypothesis (rectangle)
  trueH <- teacher_data[1, ] %>%
    select(ground_truth_x1,
           ground_truth_y1,
           ground_truth_x2,
           ground_truth_y2)
  
  colnames(trueH) <- c("x1", "y1", "x2", "y2")
  
  # Coordinates of rectangle
  trueHNum <- getRectangleIndex(trueH, nRectangles = 1)
  
  # define observations
  observations <- teacher_data %>%
    select(x, y, category)
  
  colnames(observations) <- c("x", "y", "category")
  
  # Provider helpfulness condition
  if (teacher_model == "helpful"){
    tchAlpha = 1
    lnAlpha = 1
    tchLnAlpha = 1
  } else if (teacher_model == "misleading") {
    tchAlpha = -1
    lnAlpha = 1
    tchLnAlpha = 1
  } else if (teacher_model == "random") {
    tchAlpha = 0
    lnAlpha = 0
    tchLnAlpha = 0
  } else if (teacher_model == "uninformative") {
    tchAlpha = -1
    lnAlpha = -1
    tchLnAlpha = -1  }
  
  # Empty vector to fill with all of the chosen observations in a block
  obs <- NULL
  # Empty list to fill with the observations at each trial within the block.
  trialObs <- list()
  
  # set alphas based on parameters above
  tA <- which(alphas == tchAlpha)
  tchAlphaText <- returnAlpha(tchAlpha)
  tlA <- which(alphas == tchLnAlpha)
  tchLnAlphaText <- returnAlpha(tchLnAlpha)
  lA <- which(alphas == lnAlpha)
  lnAlphaText <- returnAlpha(lnAlpha)
  
  # create empty list to fill with plots
  all_plots <- NULL
  
  
  # set initial prior: prior is just the posterior from the last prior
  if (prior == "normal") {
    lnHyp$prior <- normalPrior(hyp$size) ## CHECK THIS
  }
  
  # no observations so posteriors are just priors
  tchHyp$posterior <- tchHyp$prior
  lnHyp$posterior <- lnHyp$prior
  
  
  # set up an empty list to store the teacher distributions from each trial.
  allTchPts <- list()
  
  # loop through each trial
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
    
    # rank the posteriors from highest to lowest
    posterior_rank <- rank(desc(tchPts$posterior), ties.method = "average")
    
    # save the point distribution so we can get the probability/rannk of the point
    # that was selected according to the model
    if(rank){
      rankPts <- tchPts %>%
        mutate(posterior = posterior_rank)
      allTchPts[[i]] <- rankPts
    } else {
      allTchPts[[i]] <- tchPts
    }
    
    
    # add the observation that the teacher actually chose, so that the posterior
    # distribution from the next trial
    # is based off of this chosen point.
    newPt <- observations[i, ]
    
   if (print_plot | save_plot){
     plot <- plotColourfulDistribution(obs = observations[1:i,],
                                      trueRectangle = trueH,
                                      allPts = tchPts,
                                      title = "",
                                      subtitle = i)
    
    all_plots[[i]] <- plot
   }
    # get the name and index of the point
    newPt$name <- getPointName(newPt, pts)
    newPt$index <- as.numeric(str_sub(newPt$name, start = 2))
    
    ## Update data frame of all possible to indicate that this point has been
    # chosen.
    tchPts[as.character(newPt["name"]), "selected"] = TRUE
    
    
    # step three: teacher updates their estimate of the learner's distribution
    # over hypotheses, given the point that was generated
    tchHyp <-
      updateHypotheses(allProbPts[, , tlA], consPts, newPt, tchHyp)
    
    # step four: teacher updates their estimate of the learner's distribution
    # over points, given the hypothesis distribution
    tchPts <- updatePoints(posProbPts[, , tlA],
                           newPt,
                           posterior = abs(tchHyp$posterior),
                           pts = tchPts) 
    
    ## Keep track of which points have been sampled
    obs <- rbind(obs, newPt)
    
    
    
    
  }
  if (print_plot | save_plot){
    
    plot <- ggarrange(plotlist = all_plots)
    
    if (save_plot) {
      ggsave(plot = plot, filename = here(paste0("experiment-3/data/piloting/figures/point-probs/P-",subj_name,"-",teacher_model,"-",size,"-point-probs.png")), width = 12, height = 12)
    }
    
    if(print_plot) {
      print(plot)
    }
    
  }  
  
  allTchPts
}

# get posterior of chosen points
getPointPosterior = function(point_distributions, data) {
  trial_posteriors = c()
  for (i in 1:length(data[, 1])) {
    trial_distribution <- point_distributions[[i]]
    posterior <-
      trial_distribution[trial_distribution[, "x"] == data[i, "x"] &
                           trial_distribution[, "y"] == data[i, "y"], "posterior"]
    trial_posteriors[i] <- posterior
  }
  trial_posteriors
}

# to run more efficiently, divide data into smaller chunks and run parallel in the terminal
getProviderScore = function(data, rank = FALSE, print_plot = FALSE, save_plot = TRUE){
  # get vector of participant pids
  participants <- unique(data$pid)
  
  # get vector of teacher conditions 
  t_conds <- unique(data$provider_cond)
  
  # get vector of rectangle sizes
  t_blocks <- unique(data$size)
  
  # empty data rame to fill with posterior data
  posterior_data <- NULL
  
  # This is very inefficient 
  for(sub in 1:length(participants)){
    s <- participants[sub]
    for(cond in 1:length(t_conds)){
      c <- t_conds[cond]
      for (block in 1:length(t_blocks)){
        b <- t_blocks[[block]]
        d <- data %>%
          filter(pid == s & provider_cond == c & size == b)
        dist <- getPointDistributions(teacher_data = d, prior = "flat", teacher_model = c, print_plot =print_plot, subj_name = sub, size = b, save_plot = save_plot, rank = rank)
        point_posterior <- getPointPosterior(dist, d)
        sum_posterior <- sum(abs(point_posterior))
        d_iteration <- data.frame(uid = s, provider_cond = c, size = b, prob = as.numeric(abs(point_posterior)), trial = 1:4)
        if (rank){
          save(d_iteration, file = here(paste0("experiment-3/data/derived/provider-scores/ps-ranked-p",sub,"-",c,"-",b,".rdata")))
        } else{
          save(d_iteration, file = here(paste0("experiment-3/data/derived/provider-scores/ps-p",sub,"-",c,"-",b,".rdata")))
        }
        posterior_data <- bind_rows(posterior_data, d_iteration)
      }
    }
    print(paste0(sub, " out of ", length(participants)))
  }
  posterior_data
}

# to run more efficiently, divide data into smaller chunks and run parallel in the terminal
getProviderScoreParallel = function(data, provider_model, subj_no, rank = FALSE, print_plot = FALSE, save_plot = TRUE){
  # get vector of participant pids
  pid <- unique(data$pid)
  
  if (length(pid) > 1){
    stop("More than one participant. Use non-parallel version instead.")
  }
  
  # get vector of teacher conditions 
  t_conds <- unique(data$provider_cond)
  
  # get vector of rectangle sizes
  t_blocks <- unique(data$size)
  
  # empty data frame to fill with posterior data
  posterior_data <- NULL
  
  # to make more efficient, can pre-filter the data for each so that there is fewer iterations
  for(model in 1:length(provider_model)){
    m <- provider_model[model]
    for(cond in 1:length(t_conds)){
      c <- t_conds[cond]
      if(c != m){ # only want to save plot that is modelling best responses for the condition.
        save_plot = FALSE
      }
      for (block in 1:length(t_blocks)){
        b <- t_blocks[[block]]
        d <- data %>%
          filter(provider_cond == c & size == b)
        dist <- getPointDistributions(teacher_data = d, prior = "flat", teacher_model = m, print_plot =print_plot, subj_name = subj_no, size = b, save_plot = save_plot, rank = rank)
        point_posterior <- getPointPosterior(dist, d)
        sum_posterior <- sum(abs(point_posterior)) # negative points are negative probabilities for plotting, so convert back to positive
        d_iteration <- data.frame(uid = pid, provider_cond = c, size = b, prob = as.numeric(abs(point_posterior)), model = m, trial = 1:4)
        if (rank){
          save(d_iteration, file = here(paste0("experiment-3/modelling/04_output/provider-scores/ps-ranked-p",subj_no,"-",c,"-data-",m,"-model-",b,".rdata")))
        } else{
          save(d_iteration, file = here(paste0("experiment-3/modelling/04_output/provider-scores/ps-p",subj_no,"-",c,"-data-",m,"-model-",b,".rdata")))
        }
        posterior_data <- bind_rows(posterior_data, d_iteration)
        print(paste0("block ", block, " out of ", length(t_blocks)))
      }
      print(paste0("condition ", cond, " out of ", length(t_conds)))
    }
    print(paste0("model ", model, " out of ", length(provider_model)))
  }
  posterior_data
}

modelOptimalProviderSequential = function(true_rectangles, prior = "flat") {
  H <- 10
  provider_conditions <-
    c("helpful", "misleading", "uninformative", "random")
  
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
  
  # Draw the index of a rectangle from all possible rectangles
  #colnames(observations) <- c("x", "y", "category")
  
  experiment_parameters <-
    expand.grid(provider_conditions = provider_conditions,
                rectangle_sizes = names(true_rectangles))
  
  
  # set up an empty df to store the teacher distributions from each trial.
  allTchPts <- NULL
  for (j in 1:nrow(experiment_parameters)) {
    print(experiment_parameters[j,])
     # extract iteration parameters
    teacher_model <- experiment_parameters[j, "provider_conditions"]
    rectangle_size <- experiment_parameters[j, "rectangle_sizes"]
    
    # Get true hypothesis (rectangle)
    trueH <- true_rectangles[[rectangle_size]]
    names(trueH) <- c("x1", "y1", "x2", "y2")
    
    # index of rectangle
    trueHNum <- getRectangleIndex(trueH, nRectangles = 1)
    
    # Provider helpfulness condition
    if (teacher_model == "helpful") {
      tchAlpha = 1
      lnAlpha = 1
      tchLnAlpha = 1
    } else if (teacher_model == "misleading") {
      tchAlpha = -1
      lnAlpha = 1
      tchLnAlpha = 1
    } else if (teacher_model == "random") {
      tchAlpha = 0
      lnAlpha = 0
      tchLnAlpha = 0
    } else if (teacher_model == "uninformative") {
      tchAlpha = -1
      lnAlpha = -1
      tchLnAlpha = -1
    }
    
    # Empty vector to fill with all of the chosen observations in a block
    obs <- NULL
    
    # Empty list to fill with the observations at each trial within the block.
    #trialObs <- list()
    
    # set alphas based on parameters above
    tA <- which(alphas == tchAlpha)
    tchAlphaText <- returnAlpha(tchAlpha)
    tlA <- which(alphas == tchLnAlpha)
    tchLnAlphaText <- returnAlpha(tchLnAlpha)
    lA <- which(alphas == lnAlpha)
    lnAlphaText <- returnAlpha(lnAlpha)
    
    # set initial prior: prior is just the posterior from the last prior
    if (prior == "normal") {
      lnHyp$prior <- normalPrior(hyp$size) ## CHECK THIS
    }
    
    # no observations so posteriors are just priors
    tchHyp$posterior <- tchHyp$prior
    lnHyp$posterior <- lnHyp$prior
    

    
    # no observations yet
    #obs <- NA
    # reset tchPts selected
    tchPts$selected <- FALSE
    # loop through each trial
    for (i in 1:nTrials) {
      print(i)
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
      
      
      tchPts <- tchPts %>%
        mutate(
          # identify whether point is positive or negative
          type = ifelse(posterior > 0, "positive", "negative"),
          # take the absolute value of the posterior since we don't need it for calculating point type anymore
          posterior = abs(posterior),
          # labeling columns
          provider_cond = teacher_model,
          ground_truth_x1 = trueH["x1"],
          ground_truth_x2 = trueH["x2"],
          ground_truth_y2 = trueH["y2"],
          ground_truth_y1 = trueH["y1"],
          size = rectangle_size,
          clue = i
        )
      
      
      
      # add points to larger data frame
      allTchPts <- rbind(allTchPts, tchPts)
      
      # rank the posteriors from highest to lowest
      posterior_rank <-
        rank(desc(abs(tchPts$posterior)), ties.method = "random")
      
      # extract the highest posterior point
      newPt <-
        tchPts[which(posterior_rank == 1), c("x", "y", "type")] 
      newPt <- newPt %>%
        mutate(
          name = getPointName(newPt, pts),
          index = as.numeric(str_sub(name, start = 2)),
          category = type
        )
      
      
      ## Update data frame of all possible to indicate that this point has been
      # chosen.
      tchPts[as.character(newPt["name"]), "selected"] = TRUE
      
      
      # teacher updates their estimate of the learner's distribution
      # over hypotheses, given the point that was generated
      tchHyp <-
        updateHypotheses(allProbPts[, , tlA], consPts, newPt, tchHyp)
      
      # step four: teacher updates their estimate of the learner's distribution
      # over points, given the hypothesis distribution
      tchPts <- updatePoints(posProbPts[, , tlA],
                             newPt,
                             posterior = abs(tchHyp$posterior),
                             pts = tchPts)
      
      ## Keep track of which points have been sampled
      obs <- rbind(obs, newPt)
      
    }
  }
  
  # rename to be consistent with empirical formatting
  allTchPts <- allTchPts %>%
    rename("response_x1" = x,
           "response_y1" = y)
  allTchPts
  
}
