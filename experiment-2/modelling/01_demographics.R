####### Get demographics #########
rm(list = ls())
library(here)
library(tidyverse)
load(here("experiment-2/data/derived/data_cartesian.Rdata"))


nTrials = 32

# data frame with just one observation for each participant
d_demographics <- d_cartesian %>% 
  filter(trial_index == 1) %>%
  mutate(age = as.numeric(age))

# Follow up question
follow_up <- d_demographics %>% 
  group_by(cond) %>% 
  summarise(n = n(), sum = sum(man_check)) %>%
  mutate(perc = round((sum/n)*100, 0)) %>%
  select(-sum)

follow_up

# Gender
gender <- table(d_demographics$gender, useNA = "always")
gender

# Nationality
country <- table(d_demographics$country, useNA = "always")
country

