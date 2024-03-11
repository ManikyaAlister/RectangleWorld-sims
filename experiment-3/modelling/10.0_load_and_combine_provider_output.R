library(here)
library(tidyverse)

# set up file directory
exp <- 3
directory <- paste0("experiment-",exp,"/modelling/04_output/provider-scores/")
file_list <- list.files(here(directory))
file_list_ranked <- file_list[str_detect(file_list, "ranked")]
file_list_prob <- file_list[file_list != file_list_ranked]


load(here("experiment-3/data/clean/data_teaching_cartesian.Rdata"))
learner_conds <- d_cartesian_t %>%
  group_by(pid, cond) %>%
  summarise()

#load(here("experiment-3/data/derived/provider_scores_pilot.rdata"))
loadAndCombine = function(file_list, directory, learner_conds){
  combined_df <- NULL
  
  # load all data files and combine into single data frame
  for (i in 1:length(file_list)){
    file_name <- file_list[i]
    load(here(paste0(directory,file_name)))
    p <- unique(d_iteration$uid)
    learn_cond <- as.character(learner_conds[learner_conds[,"pid"] == p,"cond"])
    if (learn_cond == "HS"){
      learn_cond <- "helpful"
    } else if (learn_cond == "RS") {
      learn_cond <- "random"
    } else if (learn_cond == "MS"){
      learn_cond <- "misleading"
    } else if (learn_cond == "US"){
      learn_cond <- "uninformative"
    }
    
    d_iteration$learn_cond <- learn_cond
    combined_df <- rbind(combined_df, d_iteration)
  }#
  
  unique_pids <- unique(combined_df$uid)
  pid_mapping <- setNames(1:length(unique_pids), unique_pids)
  combined_df$p_num <- match(combined_df$uid, names(pid_mapping))
  combined_df$p_num <- factor(combined_df$p_num)
  combined_df
}

provider_scores_all <- loadAndCombine(file_list_prob, directory, learner_conds)
provider_scores_ranked_all <- loadAndCombine(file_list_ranked, directory, learner_conds)

save(provider_scores_all, file = here("experiment-3/modelling/04_output/provider_scores_all.Rdata"))
save(provider_scores_ranked_all, file = here("experiment-3/modelling/04_output/provider_scores_ranked_all.Rdata"))

