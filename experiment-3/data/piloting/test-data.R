library(here)
library(tidyverse)
source(here("calculatingFunctions.R"))
H <- 10

# Is the data saving correctly?
load(here("experiment-3/data/clean/clean_data_teaching_self_pilot.rdata"))

categorise_obs = function(x) {
  p = c(x["response_x1"], x["response_y1"])
  r = c(x["ground_truth_x1"], x["ground_truth_y1"], x["ground_truth_x2"], x["ground_truth_y2"])
  category = ifelse(isInRectangle(p,r), "positive", "negative")
  category
}

# convert to cartesian
d_cartesian <- data_teaching %>%
  mutate(
    ground_truth_x1 = ground_truth_x1 - 1,
    ground_truth_y1 = H - ground_truth_y1,
    # no adjustment needed for ground truth x2
    ground_truth_y2 = H - (ground_truth_y2 - 1),
    response_y1= 10-(response_y1-0.5),
    response_x1 =response_x1-0.5)


# Test 1: Appropriately labeling positive and negative
testing_fun = function(data, condition, provider, rect_size) {
  
  data <- data %>%
  filter(cond == condition &
           size == rect_size & provider_cond == provider)

responses <- cbind(data$response_x1, data$response_y1)
ground_truth <-
  cbind(data$ground_truth_x1,
        data$ground_truth_y1,
        data$ground_truth_x2,
        data$ground_truth_y2)

f <- function(x)
  isInRectangle(p = x, r = ground_truth[1, ])

apply(responses, 1, f)
}

# H1 - all positive
testing_fun(d_cartesian, "HS", "helpful", "medium")

# H2 - pops, neg, pos, neg
testing_fun(d_cartesian, "HS", "helpful", "small")


# H3 - all negative
testing_fun(d_cartesian, "HS", "helpful", "large")


# Test 2: exact points: 5,7 9,7 9,9 5,9

# without Cartesian conversion: 

data_teaching %>% 
  filter(cond == "HS", provider_cond == "misleading", size == "medium") %>%
  select(response_x1, response_y1)



obs = cbind(d_cartesian[1:4,2:3], d_cartesian_t$category[1:4])
colnames(obs) = c("x", "y", "category")

# plot participant responses 
plotHypotheses(r = d_cartesian_t[1,8:11], obs = obs)