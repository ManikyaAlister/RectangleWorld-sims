# 02_prepare_model_runs.R creates a separate run of this script for each condition based on the 
# on the parameters in all_conditions. Each run corresponds to condId which is called in that script too. 
# set up parallel model runs
library(here)
source(here("getLearnerHypDistributions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))


# comes from run-scripts.sh
condId <- commandArgs(trailingOnly = TRUE)

tb <- all_conditions[condId,"targetBlocks"]
cond <- all_conditions[condId,"conditions"]
clue <- all_conditions[condId,"clues"]

load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",tb,"-Cartesian.Rdata")))
load(here(paste0("experiment-1/data/derived/",cond,"-clue-",clue,"-tb-",tb,".Rdata")))

observations <- targetBlock$observations

posteriors <- getMultiAlphaPosteriors(learnerRectangles = d_cond, observations = observations, prior = "flat")

save(posteriors, file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat.Rdata")))


