rm(list = ls())
library(here)
library(tidyverse)
exp = 3
source(here("functions/calculatingFunctions.R"))
load(here(paste0("experiment-",exp,"/data/derived/all_conditions.Rdata")))
load(here(paste0("experiment-",exp,"/data/derived/data_cartesian.Rdata")))

# adjust all_conditions so that it is agnostic to clue number.
blocks <- 1:8

# All blocks 
for (i in blocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  # calculate the probability of each hypothesis and save it. 
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = exp) 
  print(paste0("Block ", i, " finished"))
}

 