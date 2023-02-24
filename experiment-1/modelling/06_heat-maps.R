rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

tb2 <- all_conditions %>% filter(targetBlocks == 2)
plotHeatMaps(d = d_cartesian, all_conditions = tb2, experiment = 1)

tb8 <- all_conditions %>% filter(targetBlocks == 8)
plotHeatMaps(d = d_cartesian, all_conditions = tb8, experiment = 1)


