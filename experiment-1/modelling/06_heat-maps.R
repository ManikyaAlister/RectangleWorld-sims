rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)

source(here("functions/plottingFunctions.R"))
source(here("functions/calculatingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.Rdata"))
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

blocks <- c(2,8)
# All blocks 
for (i in blocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  #getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = 1)
  plotHeatMaps(all_conditions = block_conds, experiment = 1)
}




