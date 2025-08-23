# Import experiment data from google cloud
# Python3 export_results.py live rectangleworld learn c23 v3 results.json

rm(list = ls())
library(here)
library(jsonlite)
library(tidyverse)

nClues <- 4
nBlocks <- 8

# load data
d_json <- read_json(here("experiment-1-rerun/data/raw/results.json"))
test <- d_json[[1]]

cleaning_fun = function(raw_data, nClues, nBlocks) {
  raw_data <- raw_data[[1]]
  pid_all <- names(raw_data)
  data <- NULL
  for (i in 1:length(pid_all)) {
    d_participant <- raw_data[[i]]
    # skip if participant didn't finish
    if (is.null(d_participant$experimentEndStatus))
      next
    # skip if participant isn't an mturker
    if (d_participant$src != "mt")
      next
    # convert data from list to single column matrix
    d_df <- as.matrix(unlist(d_participant))
    # get participant responses.
    raw_response_x1 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-x0"), ])
    raw_response_y1 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-y0"), ])
    raw_response_x2 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-x1"), ])
    raw_response_y2 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-y1"), ])
    
    # For modelling, coordinates need to be ordered such that smallest x/y is x1/y1
    response_x1 <- c()
    response_x2 <- c()
    response_y1 <- c()
    response_y2 <- c()
    
    for (j in 1:length(raw_response_x1)) {
      response_x1[j] <- min(raw_response_x1[j], raw_response_x2[j])
      response_x2[j] <- max(raw_response_x1[j], raw_response_x2[j])
      # because y is inverted in grid world (e.g., indexes from the top) it's the opposite (largest as y1)
      response_y1[j] <- max(raw_response_y1[j], raw_response_y2[j])
      response_y2[j] <- min(raw_response_y1[j], raw_response_y2[j])
    }
    
    
    # get ground truths
    ground_truth_x1 <-
      d_df[str_detect(rownames(d_df), "groundTruth-x1"), ]
    ground_truth_y1 <-
      d_df[str_detect(rownames(d_df), "groundTruth-y1"), ]
    ground_truth_x2 <-
      d_df[str_detect(rownames(d_df), "groundTruth-x2"), ]
    ground_truth_y2 <-
      d_df[str_detect(rownames(d_df), "groundTruth-y2"), ]
    
    trial_index <-
      as.numeric(d_df[str_detect(rownames(d_df), "trialIndex"), ]) + 1 # plus 1 to convert from 0 base to 1 base
    
    # participant id
    pid <- rep(pid_all[i], (nBlocks * nClues))
    # demographics
    age <- d_participant$demographics_age
    if (is.null(age))
      age <- NA
    gender <- d_participant$demographics_gender
    if (is.null(gender))
      gender <- NA
    country <- d_participant$demographics_country
    if (is.null(country))
      country <- NA
    
    experiment_end_time <-
      as.numeric(d_participant$experimentEndTime)
    experiment_start_time <-
      as.numeric(d_participant$experimentStartTime)
    
    # Trial tine
    trial_start <-
      d_df[str_detect(rownames(d_df), "trialStartTime"), ]
    trial_end <- d_df[str_detect(rownames(d_df), "trialEndTime"), ]
    trial_iqr <-
      IQR(as.numeric(trial_end) - as.numeric(trial_start)) / 1000
    
    
    # function to get n_cover_check from a data frame
    get_n_cover_check <- function(df) {
      # get all rownames
      rn <- rownames(df)
      
      # find those that match coverCheck_endTime_#
      cover_rows <- grep("^coverCheck_endTime_[0-9]+$", rn, value = TRUE)
      
      if (length(cover_rows) == 0) {
        return(0)  # no cover check rows
      }
      
      # extract the numbers after the final underscore
      attempts <- as.integer(sub(".*_", "", cover_rows))
      
      # maximum attempt
      max(attempts, na.rm = TRUE)
    }
    
    # example usage
    n_cover_check <- get_n_cover_check(d_df)
    #print(n_cover_check)
    
    
    
    follow_up <-
      d_participant$`RWLearningPhase_T-31-t10-3_clueGenerationFollowup`
    if (is.null(follow_up))
      follow_up <- NA
    completed <- d_participant$experimentEndStatus
    
    # get condition data
    cond <- d_participant$condition
    
    # combine into single data frame 
    d_clean <-
      cbind(
        pid,
        response_x1,
        response_y1,
        response_x2,
        response_y2,
        cond,
        trial_index,
        ground_truth_x1,
        ground_truth_y1,
        ground_truth_x2,
        ground_truth_y2,
        age,
        gender,
        country,
        follow_up,
        experiment_end_time,
        trial_iqr,
        n_cover_check,
        completed
      )
    rownames(d_clean) <- NULL
    d_clean <- as.data.frame(d_clean)
    # experiment saves data in funny order, so need to re-order from the trial index
    d_clean$trial_index <- as.numeric(d_clean$trial_index)
    d_clean <- d_clean %>%
      arrange(trial_index)
    # get number in block
    d_clean$clue <- rep(c(1:nClues), nBlocks)
    # get blocks
    d_clean$block <-
      c(
        rep(1, nClues),
        rep(2, nClues),
        rep(3, nClues),
        rep(4, nClues),
        rep(5, nClues),
        rep(6, nClues),
        rep(7, nClues),
        rep(8, nClues)
      )
    
    data <- rbind(data, d_clean)
  }
  # change multiple columns to numeric
  data <-  data %>%
    mutate_at(vars(
      c(
        ground_truth_x1,
        ground_truth_y1,
        ground_truth_x2,
        ground_truth_y2,
        response_x1,
        response_y1,
        response_x2,
        response_y2,
        trial_index,
        trial_iqr,
        n_cover_check
      )
    ), as.numeric)
  
  data
}



data <- cleaning_fun(d_json, nBlocks = nBlocks, nClues = nClues)

save(data, file =  here("experiment-1-rerun/data/clean/clean_data.rdata"))

