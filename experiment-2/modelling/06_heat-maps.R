rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)

exp <- 3

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here(paste0("experiment-",exp,"/data/derived/all_conditions.Rdata")))
load(here(paste0("experiment-",exp,"/data/derived/data_cartesian.Rdata")))

blocks <- 8
# All blocks 
for (i in blocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = exp)
  plotHeatMaps(all_conditions = block_conds, experiment = exp)
}




