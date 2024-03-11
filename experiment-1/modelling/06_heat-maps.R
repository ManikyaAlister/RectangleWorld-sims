rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.Rdata"))
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

blocks <- 8
# All blocks 
for (i in blocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = 1)
  plotHeatMaps(all_conditions = block_conds, experiment = 1)
}




