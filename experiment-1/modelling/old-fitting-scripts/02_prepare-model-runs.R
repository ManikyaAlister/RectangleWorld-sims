# set up parallel model runs
library(here)
source(here("getLearnerHypDistributions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))


nConditions <- length(all_conditions[,1])

for( i in 1:nConditions){
  condId <- i 
  rstudioapi::jobRunScript("experiment-1/modelling/03_fit_model.R", importEnv = TRUE)
}

