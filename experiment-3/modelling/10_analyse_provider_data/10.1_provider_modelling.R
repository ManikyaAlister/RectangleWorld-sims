rm(list=ls())
library(here)
library(parallel)  
library(doParallel)

# source modelling functions
source(here("functions/modelProviders.R"))

# load data
load(here("experiment-3/data/clean/data_teaching_cartesian.Rdata"))

d_cartesian_t <- d_cartesian_t %>%
  rename("x" = response_x1, "y" = response_y1)

# Get participant IDs
PIDs <- unique(d_cartesian_t$pid)
n_subj <- length(PIDs)

# Determine number of cores
num_cores <- detectCores() - 2

# Create cluster
cl <- makeCluster(num_cores) 

# Register cluster
registerDoParallel(cl)

foreach (i = 1:n_subj, .packages=c('here', 'tidyverse','ggpubr')) %dopar% {
  
  subject_no <- i
  
  subject <- PIDs[subject_no]
  
  # Filter data by subject
  data <- d_cartesian_t %>% 
    filter(pid == subject)
  
  #score <- getProviderScoreParallel(data, c("helpful", "random", "misleading", "uninformative"), subject_no, rank = FALSE)
  score_ranked <- getProviderScoreParallel(data, c("helpful", "misleading", "uninformative"), subject_no, rank = TRUE)
  
  
}

# Stop cluster
stopCluster(cl)



#provider_scores <- getProviderScore(d_cartesian_t)
#save(provider_scores, file = here("experiment-3/data/derived/provider-scores/all_provider_scores.rdata"))


# Ordinal probs ----------------------------------------------------------
# In some conditions, no single point is worth that much. 
# For example, the corner points in the helpful conditions have a way higher prob
# relative to other points. But there are a lot more points that have relatively 
# similar probability in the misleading condition. One way to lessen this problem
# is to look at the rank of points instead of their raw probability.

#provider_scores_ranked <- getProviderScore(d_cartesian_t, rank = TRUE, print_plot = FALSE, save_plot = FALSE)
#save(provider_scores, file = here("experiment-3/data/derived/provider-scores/all_provider_scores.rdata"))



