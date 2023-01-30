library(here)
library(tidyverse)
source(here("plottingFunctions.R"))

exp = 2

# Target block 2 ----------------------------------------------------------
load(here("experiment-1/modelling/04_output/tb2-all-alpha-posteriors.Rdata"))

sum = all_alpha_posteriors %>%
  group_by(alpha, cond, clue) %>%
  summarise(mean = mean(posterior), median = median(posterior), sum = sum(posterior), Probability = median(posterior/sum(median(posterior))))%>%
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

plotPosteriors(data = sum, exp = exp, statistic =  "mean", 2)
plotPosteriors(data = sum, exp = exp, statistic =  "median", 2)
plotPosteriors(data = sum, exp = exp, statistic =  "sum", 2)

# only participants who passed the manipulation check 
sum = all_alpha_posteriors %>%
  filter(man_check == TRUE & cond %in% c("MS", "US", "RS", "HS")) %>%
  group_by(alpha, cond, clue) %>%
  summarise(mean = mean(posterior), median = median(posterior), sum = sum(posterior)) %>%
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

plotPosteriors(data = sum, exp = exp, statistic =  "mean", 2, man_check = TRUE)
plotPosteriors(data = sum, exp = exp, statistic =  "median", 2, man_check = TRUE)
plotPosteriors(data = sum, exp = exp, statistic =  "sum", 2, man_check = TRUE)



# Target block 8 ----------------------------------------------------------
load(here("experiment-1/modelling/04_output/tb8-all-alpha-posteriors.Rdata"))

exp = 2

sum = all_alpha_posteriors %>%
  group_by(alpha, cond, clue) %>%
  summarise(mean = mean(posterior), median = median(posterior), sum = sum(posterior)) %>%
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

plotPosteriors(data = sum, exp = exp, statistic =  "mean", 8)
plotPosteriors(data = sum, exp = exp, statistic =  "median", 8)
plotPosteriors(data = sum, exp = exp, statistic =  "sum", 8)


# only participants who passed the manipulation check 
sum = all_alpha_posteriors %>%
  filter(man_check == TRUE & cond %in% c("MS", "US", "RS", "HS")) %>%
  group_by(alpha, cond, clue) %>%
  summarise(mean = mean(posterior), median = median(posterior), sum = sum(posterior)) %>%
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

plotPosteriors(data = sum, exp = exp, statistic =  "mean", 8, man_check = TRUE)
plotPosteriors(data = sum, exp = exp, statistic =  "median", 8, man_check = TRUE)
plotPosteriors(data = sum, exp = exp, statistic =  "sum", 8, man_check = TRUE)

