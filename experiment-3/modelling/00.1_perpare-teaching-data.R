library(here)
library(tidyverse)
library(ggpubr)
rm(list = ls())
# load pilot data 
load(here("experiment-3/data/clean/clean_data_teaching_turk_pilot.rdata"))
pilot_data <- data_teaching
pilot_subjs <- unique(pilot_data$pid)


load(here("experiment-3/data/clean/clean_data_teaching.rdata"))
load(here("experiment-3/data/clean/clean_data.rdata"))
source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
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
  filter(!pid %in% pilot_subjs) %>% # make sure pilot participants are not included
  mutate(
    ground_truth_x1 = ground_truth_x1-1,
    ground_truth_y1 = H-ground_truth_y1,
    # no adjustment needed for ground truth x2
    ground_truth_y2 = H-(ground_truth_y2-1),
    response_y1= 10-(response_y1-0.5),
    response_x1 =response_x1-0.5) %>%
  arrange(pid, provider_cond, trial_index)

d_cartesian_t$category = apply(d_cartesian_t, 1, categorise_obs)

# get participant numbers
unique_pids <- unique(d_cartesian_t$pid)
pid_mapping <- setNames(1:length(unique_pids), unique_pids)
d_cartesian_t$p_num <- match(d_cartesian_t$pid, names(pid_mapping))

save(d_cartesian_t, file = here("experiment-3/data/clean/data_teaching_cartesian.Rdata"))