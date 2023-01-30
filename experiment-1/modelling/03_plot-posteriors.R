rm(list = ls())
library(here)
library(tidyverse)
library(RColorBrewer)
source(here("plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))
exp = 1

# Target block 8 ----------------------------------------------------------

load(here(paste0("experiment-",exp,"/modelling/04_output/tb8-all-alpha-posteriors.Rdata")))

all_conditions <- all_conditions %>% 
  filter(targetBlocks == 8 & clues == 4)

for (i in 1:length(all_conditions[,1])){
  condition <- all_conditions[i,"conditions"]
  all_data <- all_alpha_posteriors %>% 
    filter(cond == condition & clue == 4) %>% 
    mutate(alpha = as.factor(alpha))
  sum <- all_data %>% 
    group_by(alpha, cond, clue) %>%
    summarise(mean = mean(posterior), median = median(posterior), sum = sum(posterior))
  plotPosteriors(sum_data = sum, all_data = all_data, exp = exp, statistic = "median", 8)
  ggsave(here(paste0("experiment-1/modelling/05_plots/posteriors-",condition,".png")), width = 7, height = 5)
}

