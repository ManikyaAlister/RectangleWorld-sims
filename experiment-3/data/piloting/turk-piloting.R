library(here)
library(tidyverse)
library(ggpubr)
rm(list = ls())
load(here("experiment-3/data/clean/clean_data_teaching_turk_pilot.rdata"))
load(here("experiment-3/data/clean/clean_data_turk_pilot.rdata"))
source(here("functions/plottingFunctions.R"))
source(here("functions/calculatingFunctions.R"))
# check average run time
H = 10
run_time <- data %>% 
  group_by(pid) %>%
  summarise(minutes = mean(as.numeric(experiment_end_time)/60000))

categorise_obs = function(x) {
  p = c(x["response_x1"], x["response_y1"])
  r = c(x["ground_truth_x1"], x["ground_truth_y1"], x["ground_truth_x2"], x["ground_truth_y2"])
  category = ifelse(isInRectangle(p,r), "positive", "negative")
  category
}

# convert to cartesian
d_cartesian_t <- data_teaching %>%
  mutate(
    ground_truth_x1 = ground_truth_x1-1,
    ground_truth_y1 = H-ground_truth_y1,
    # no adjustment needed for ground truth x2
    ground_truth_y2 = H-(ground_truth_y2-1),
    response_y1= 10-(response_y1-0.5),
    response_x1 =response_x1-0.5) %>%
  arrange(pid, provider_cond, trial_index)

d_cartesian_t$category = apply(d_cartesian_t, 1, categorise_obs)

save(d_cartesian_t, file = here("experiment-3/data/clean/data_teaching_cartesian.Rdata"))

# 15 obviously an outlier, so remove them
run_time_or <- run_time[-15,]
paste0("mean runtime: ", mean(run_time_or$minutes))
paste0("median runtime: ", median(run_time_or$minutes))

# plot responses
sizes = c("medium","small", "large")
conds = c("helpful","misleading","uninformative")


plot_teaching_trials = function(participant, data){
  participants <- unique(data$pid)
  n = length(unique(data$pid))
  data <- data %>% 
    filter(pid == participants[participant])
  
  plot_list = list()
  for(i in 1:length(conds)){
    teacher_cond <- conds[i]
    for(j in 1:length(sizes)) {
      if (i == 1 & j == 1){
        main = paste0("Participant ", participant)
      } else {
        main = ""
      }
      tmp_list <- c()
      rect <- sizes[j]
      d <- data %>%
        filter(provider_cond==teacher_cond & size == rect)
      obs = cbind(d[,2:3], d$category)
      colnames(obs) = c("x", "y", "category")
      trueRect <- d[1,8:11]
      plot <- plotHypothesesTeach(r = trueRect, obs = obs, title = main, subtitle = teacher_cond)
      tmp_list[[1]] <- plot
      plot_list <- c(plot_list, tmp_list) 
    }
    
  }
  
  all_plots <- ggarrange(plotlist = plot_list )
  ggsave(plot = all_plots, width = 12, height = 12, filename = here(paste0("experiment-3/data/piloting/figures/teaching_responses_P",participant,".png")))
  all_plots
}

for (i in 1:length(unique(d_cartesian_t$pid))){
  plot_teaching_trials(i, d_cartesian_t)
}

