####### Get demographics #########
rm(list = ls())
library(here)
library(tidyverse)
load(here("experiment-1/data/derived/data_cartesian.Rdata"))


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

# Age
age <- d_demographics$age
age_dems <- c(min = min(age, na.rm = TRUE), max = max(age, na.rm = TRUE), mean = mean(age, na.rm = TRUE), sd = sd(age, na.rm = TRUE))
age_dems

# Gender
gender <- table(d_demographics$gender)
gender_perc <- round(gender/sum(gender)*100,0)
gender_perc

# Nationality
nationality <- table(d_demographics$country)
nationality_perc <- round((nationality/sum(nationality))*100,0)
nationality_perc

nationality_unique <- length(unique(d_demographics$country))
nationality_unique
