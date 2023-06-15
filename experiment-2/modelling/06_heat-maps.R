rm(list = ls())
library(here)
library(tidyverse)

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-2/data/derived/all_conditions.Rdata"))
load(here("experiment-2/data/derived/data_cartesian.Rdata"))

source(here("fixing_heatmaps.R"))


# Target blocks only 

#tb2 <- all_conditions %>% filter(blocks == 2)
#plotHeatMaps(d = d_cartesian, all_conditions = tb2, experiment = 2)

#tb8 <- all_conditions %>% filter(blocks == 8)
#plotHeatMaps(d = d_cartesian, all_conditions = tb8, experiment = 2)



nBlocks <- 8
# All blocks 
for (i in 1:nBlocks) {
  block_conds <- all_conditions %>%
    filter(blocks == i)
  getHypProbs(d = d_cartesian, all_conditions = block_conds, experiment = 2)
  plotHeatMaps2(all_conditions = block_conds, experiment = 2)
}



