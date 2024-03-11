rm(list = ls())
library(here)
library(tidyverse)

#source(here("plottingFunctions.R"))
source(here("plottingFunctions.R"))
#source(here("fixing_heatmaps.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-3/data/derived/all_conditions.Rdata"))
load(here("experiment-3/data/derived/data_cartesian.Rdata"))

load(here("experiment-3/modelling/11.1_filtered-analyses-conservative/good_uid_conservative.Rdata"))

d_cartesian <- d_cartesian %>%
  filter(pid %in% good_uid_conservative)

blocks <- 8
# All blocks 
for (i in blocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = 3)
  plotHeatMaps(all_conditions = block_conds, experiment = 3, filtered = TRUE)
}



