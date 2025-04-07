# 02_prepare_model_runs.R creates a separate run of this script for each condition based on the 
# on the parameters in all_conditions. Each run corresponds to condId which is called in that script too. 
# set up parallel model runs
library(here)
library(tidyverse)
source(here("functions/getLearnerHypDistributions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

# only interested in cover story conditions, because they should have passed the manipulation check. 
csConds <- c("MS","US", "RS", "HS")

all_conditions <- all_conditions %>%
  filter(conditions %in% csConds)

# comes from run-scripts.sh
condId <- commandArgs(trailingOnly = TRUE)

tb <- all_conditions[condId,"targetBlocks"]
cond <- all_conditions[condId,"conditions"]
clue <- all_conditions[condId,"clues"]

load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",tb,"-Cartesian.Rdata")))
load(here(paste0("experiment-1/data/derived/",cond,"-clue-",clue,"-tb-",tb,".Rdata")))

# only participants who passed the manipulation check
d_cond_mc <- d_cond %>% 
  filter(man_check == TRUE)

# keep track of how many participants were removed
load(here("experiment-1/data/derived/n_rm.Rdata"))
n_rm[condId] <- length(d_cond[,1]) - length(d_cond_mc[,1])

# define observations
observations <- targetBlock$observations

# get posterior probability of each rectangle given observations for multiple alphas
posteriors <- getMultiAlphaPosteriors(learnerRectangles = d_cond, observations = observations, prior = "flat")

save(posteriors, file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
