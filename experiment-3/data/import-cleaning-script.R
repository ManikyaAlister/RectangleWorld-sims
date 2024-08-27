# Import experiment data from google cloud
# Python3 export_results.py live rectangleworld learn c23 v2 results.json

rm(list = ls())
library(here)
library(jsonlite)
library(tidyverse)

nClues <- 5 # 4 clues + 1 prior trial each block
nBlocks <- 8

# load data
d_json <- read_json(here("experiment-3/data/raw/results.json"))
test <- d_json[[1]]

t <- as.character(unname(as.matrix(sapply(test, function(x) x["?mtWorkerId"]))))

# function to clean learning phase data
cleaning_fun_learning = function(raw_data, nClues, nBlocks) {
  raw_data <- raw_data[[1]]
  all_mturk <- NULL 
  pid_all <- names(raw_data)
  data <- NULL
  for (i in 1:length(pid_all)) {
    d_participant <- raw_data[[i]]
    # skip if participant didn't finish
    if (is.null(d_participant$experimentEndStatus))
      next
    if (is.null(d_participant$`RWTeachingPhase_2-T-11-M_3-3_clue`))
      next
    if (d_participant$`RWTeachingPhase_2-T-11-M_3-3_clue` == "NaN")
      next
    # skip if participant isn't an mturker
    if (d_participant$src != "mt")
      next
    # convert data from list to single column matrix
    d_df <- as.matrix(unlist(d_participant))
    
    # remove teaching phase data 
    teaching_trials <- str_detect(rownames(d_df), "RWTeaching")
    d_df <- as.matrix(d_df[!teaching_trials,])
    
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
    
    mturk_id <- NULL # clear variable
   
     # get mturker id
    mturk_id <- d_participant$`?mtWorkerId`
    
    # the "?" at the start of mtWorkerId is an error fixed in the second round of collection
    if (is.null(mturk_id))  {
      mturk_id <- d_participant$`mtWorkerId`
    }

    
    
    # get all mturk ids so we can look for duplicates 
    all_mturk <- c(all_mturk, mturk_id)
    
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
    
    
    
    follow_up <-
      d_participant$`RWLearningPhase_T-39-t10-4_clueGenerationFollowup`
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
        completed,
        mturk_id
      )
    rownames(d_clean) <- NULL
    d_clean <- as.data.frame(d_clean)
    # experiment saves data in funny order, so need to re-order from the trial index
    d_clean$trial_index <- as.numeric(d_clean$trial_index)
    d_clean <- d_clean %>%
      arrange(trial_index)
    # get number in block
    d_clean$clue <- rep(c(1:nClues)-1, nBlocks)
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
  
  # figure out which mturkers are duplicates
  duplicates <- all_mturk[duplicated(all_mturk) | duplicated(all_mturk, fromLast = TRUE)]
  
  print(paste0("Removing ", length(duplicates), " instances from participats who completed multiple times"))  

  # change multiple columns to numeric
  data <-  data %>%
    filter(!mturk_id %in% duplicates) %>%
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
        trial_iqr
      )
    ), as.numeric)
  
  data
}

# function to clean teaching phase data
cleaning_fun_teaching = function(raw_data, nClues, nBlocks) {
  raw_data <- raw_data[[1]]
  all_mturk <- NULL 
  pid_all <- names(raw_data)
  n_conds = 3
  data <- NULL
  for (i in 1:length(pid_all)) {
    d_participant <- raw_data[[i]]
    # skip if participant didn't finish
    if (is.null(d_participant$experimentEndStatus))
      next
     if (is.null(d_participant$`RWTeachingPhase_2-T-11-M_3-3_clue`))
       next
    if (d_participant$`RWTeachingPhase_2-T-11-M_3-3_clue` == "NaN")
      next
    # skip if participant isn't an mturker
    if (d_participant$src != "mt")
      next
    # convert data from list to single column matrix
    d_df <- as.matrix(unlist(d_participant))
    
    # remove learning phase data 
    learning_trials <- str_detect(rownames(d_df), "RWLearning")
    d_df <- as.matrix(d_df[!learning_trials,])
    
    # get participant responses.
  response_x1 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-x0"), ])
   response_y1 <-
      as.numeric(d_df[str_detect(rownames(d_df), "response-y0"), ])
    
   # get ground truths
   raw_ground_truth_x1 <-
     d_df[str_detect(rownames(d_df), "groundTruth-x1"), ]
   raw_ground_truth_y1 <-
     d_df[str_detect(rownames(d_df), "groundTruth-y1"), ]
   raw_ground_truth_x2 <-
     d_df[str_detect(rownames(d_df), "groundTruth-x2"), ]
   raw_ground_truth_y2 <-
     d_df[str_detect(rownames(d_df), "groundTruth-y2"), ]
   
   # For modelling, rectangle coordinates need to be ordered such that smallest x/y is x1/y1
   ground_truth_x1 <- c()
   ground_truth_x2 <- c()
   ground_truth_y1 <- c()
   ground_truth_y2 <- c()
   
   for (j in 1:length(raw_ground_truth_x1)) {
     ground_truth_x1[j] <- min(raw_ground_truth_x1[j], raw_ground_truth_x2[j])
     ground_truth_x2[j] <- max(raw_ground_truth_x1[j], raw_ground_truth_x2[j])
     # because y is inverted in grid world (e.g., indexes from the top) it's the opposite (largest as y1)
     ground_truth_y1[j] <- max(raw_ground_truth_y1[j], raw_ground_truth_y2[j])
     ground_truth_y2[j] <- min(raw_ground_truth_y1[j], raw_ground_truth_y2[j])
   }

    
    provider_cond <- d_df[str_detect(rownames(d_df), "provider-cond"), ]
    
    trial_index <-
      as.numeric(d_df[str_detect(rownames(d_df), "trialIndex"), ]) + 1 # plus 1 to convert from 0 base to 1 base
    
    # participant id
    pid <- rep(pid_all[i], (nBlocks * nClues*n_conds))
    
    # reset mturk variable
    mturk_id <- NULL
    
    # get mturker id
    mturk_id <- d_participant$`?mtWorkerId`
    
    # due to an error caught in an earlier run of the experiment, sometimes mt field is saved without "?"
    if (is.null(mturk_id))  mturk_id <- d_participant$`mtWorkerId`
    
    # get all mturk ids so we can look for duplicates 
    all_mturk <- c(all_mturk, mturk_id)
    
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
    
    # get trial ID
    trial_id <- d_df[str_detect(rownames(d_df), "_id"), ]
    
    result <- str_match(trial_id, "([^-]+)_([^-]+)") # return the characters around the "_"
    
    # get rectangle size
    rectangle_sizes <- c("medium", "small", "large") # this order corresponds with the experiment
    size <- rectangle_sizes[as.numeric(result[,3])] # the third index of result corresponds to the rectangle order
    
    
    # Trial tine
    trial_start <-
      d_df[str_detect(rownames(d_df), "trialStartTime"), ]
    trial_end <- d_df[str_detect(rownames(d_df), "trialEndTime"), ]
    trial_iqr <-
      IQR(as.numeric(trial_end) - as.numeric(trial_start)) / 1000
    
    
    
    # follow_up <-
    #   d_participant$`RWLearningPhase_T-39-t10-4_clueGenerationFollowup`
    # if (is.null(follow_up))
    #   follow_up <- NA
    completed <- d_participant$experimentEndStatus
    
    # get condition data
    cond <- d_participant$condition
    
    # combine into single data frame 
    d_clean <-
      cbind(
        pid,
        response_x1,
        response_y1,
        cond,
        provider_cond,
        trial_index,
        size,
        ground_truth_x1,
        ground_truth_y1,
        ground_truth_x2,
        ground_truth_y2,
        age,
        gender,
        country,
        #follow_up,
        experiment_end_time,
        trial_iqr,
        completed,
        mturk_id
      )
    rownames(d_clean) <- NULL
    d_clean <- as.data.frame(d_clean)
    # get clue number 
    d_clean$clue <- as.numeric(d_df[str_detect(rownames(d_df), "clue"), ])
    
    # experiment saves data in funny order, so need to re-order from the trial index
    d_clean$trial_index <- as.numeric(d_clean$trial_index)
    d_clean <- d_clean %>%
      arrange(provider_cond)

    # get blocks
    # d_clean$block <-
    #   rep(rep(1:nBlocks, each= nClues),n_conds) # 3 teacher conditions
    # 
    data <- rbind(data, d_clean)
  }
  # figure out which mturkers are duplicates
  duplicates <- all_mturk[duplicated(all_mturk) | duplicated(all_mturk, fromLast = TRUE)]

  print(paste0("Removing ", length(duplicates), " instances from participats who completed multiple times"))  
  
  # change multiple columns to numeric
  data <-  data %>%
    filter(!mturk_id %in% duplicates) %>%
    mutate_at(vars(
      c(
        ground_truth_x1,
        ground_truth_y1,
        ground_truth_x2,
        ground_truth_y2,
        response_x1,
        response_y1,
        trial_index,
        trial_iqr
      )
    ), as.numeric)
  
  data
}

removePilotData <- function(full_data){
  load(here("experiment-3/data/clean/clean_data_turk_pilot.rdata"))
  pilot_pid <- unique(data$pid)
  clean_data <-  full_data %>%
    filter(!pid %in% pilot_pid)
  print(paste0("removing ", length(pilot_pid), " pilot participants from main sample"))
  print(paste0("final sample: ", length(unique(clean_data$pid))))
  clean_data
}

data <- removePilotData(cleaning_fun_learning(d_json, nBlocks = nBlocks, nClues = nClues))
data_teaching <- removePilotData(cleaning_fun_teaching(d_json, nBlocks = 3, nClues = 4))






tmp = data %>%
  group_by(pid) %>%
  summarise(mturk = unique(mturk_id), cond = unique(cond))

save(data, file =  here("experiment-3/data/clean/clean_data.rdata"))
save(data_teaching, file =  here("experiment-3/data/clean/clean_data_teaching.rdata"))


