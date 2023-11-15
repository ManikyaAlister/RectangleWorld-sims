library(here)
library(tidyverse)


# General performance in provider phase -----------------------------------

# set up file directory
exp <- 3
directory <- paste0("experiment-",exp,"/modelling/04_output/provider-scores/")
file_list <- list.files(here(directory))
file_list_ranked <- file_list[str_detect(file_list, "ranked")]
file_list_prob <- file_list[file_list != file_list_ranked]

#load(here("experiment-3/data/derived/provider_scores_pilot.rdata"))
loadAndCombine = function(file_list, directory){
  combined_df <- NULL
  
  # load all data files and combine into single data frame
  for (i in 1:length(file_list)){
    file_name <- file_list[i]
    load(here(paste0(directory,file_name)))
    combined_df <- rbind(combined_df, d_iteration)
  }
  
  unique_pids <- unique(combined_df$uid)
  pid_mapping <- setNames(1:length(unique_pids), unique_pids)
  combined_df$p_num <- match(combined_df$uid, names(pid_mapping))
  combined_df$p_num <- factor(combined_df$p_num)
  combined_df
}

provider_scores_all <- loadAndCombine(file_list_prob, directory)
provider_scores_ranked_all <- loadAndCombine(file_list_ranked, directory)

provider_scores <- provider_scores_all %>%
  filter(model == provider_cond)

provider_scores_ranked <- provider_scores_ranked_all %>%
  filter(model == provider_cond)
# Just corresponding model ------------------------------------------------


# inspect provider scores on their own
plotProviderScores = function(provider_scores, ranked = FALSE){
  
  sum_provider_scores <- provider_scores %>%
    group_by(p_num, provider_cond) %>%
    summarise(prob= mean(as.numeric(prob))) 
if (ranked) {
  plot <- provider_scores %>% 
    ggplot(aes(x = p_num, y = 100-as.numeric(prob)))+
    geom_col(data = sum_provider_scores)+
    geom_point(aes(colour = size))+
    labs(y = "Reverse rank (100 best)")+
    facet_wrap(~provider_cond)
} else {
  plot <- provider_scores %>% 
    ggplot(aes(x = p_num, y = as.numeric(prob)))+
    geom_col(data = sum_provider_scores)+
    geom_point(aes(colour = size))+
    facet_wrap(~provider_cond)
}
  
  
  plot
}

plotProviderScores(provider_scores)


# Because there is higher posterior density allocated to certain points in 
# different conditions, it makes sense to look at this in an ordinal way. In other words,
# what rank was the point they chose? 

plotProviderScores(provider_scores_ranked, ranked = TRUE)


# Correlation between provider and learner phases -------------------------


getProviderLearnerCorr = function(provider_scores, alpha, blocks = 8, recursive = FALSE){
  
  if (alpha > 0 ){
    cond <- "helpful"
  } else if (alpha == 0){
    cond <- "random"
  } else if (alpha < 0 & recursive == TRUE) {
    cond <- "misleading"
  } else {
    cond <- "uninformative"
  }
  
  learner_posteriors <- NULL
  
  for (b in blocks) {
    load(here(paste0("experiment-3/modelling/04_output/b",b,"-all-alpha-posteriors-",cond,".Rdata")))
    learner_posteriors <- rbind(learner_posteriors, all_alpha_posteriors)
  }
  # get participants who were in each condition
  participants <- unique(learner_posteriors$pid)
  
  p_provider <- provider_scores %>%
    filter(uid %in% participants & provider_cond == cond) %>%
    # get 1 score for each participant
    group_by(uid)%>%
    summarise(mean = mean(prob))
  
  p_learner <- learner_posteriors %>%
    filter(alpha == alpha) %>%
    group_by(pid)%>%
    summarise(mean = mean(posterior))
  print(cor(p_provider$mean, p_learner$mean))  
  plot(p_provider$mean, p_learner$mean)  
}

# load learning phase data
blocks <- 8

getProviderLearnerCorr(provider_scores, -1, blocks = blocks, recursive = TRUE)
getProviderLearnerCorr(provider_scores_ranked, -1, blocks = blocks, recursive = FALSE)


# next do ordinal version
