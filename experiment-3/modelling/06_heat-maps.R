rm(list = ls())
library(here)
library(tidyverse)

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-3/data/derived/all_conditions.Rdata"))
load(here("experiment-3/data/derived/data_cartesian.Rdata"))

nBlocks <- 8
# All blocks 
for (i in 1:nBlocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = 3)
  plotHeatMaps(all_conditions = block_conds, experiment = 3)
}



