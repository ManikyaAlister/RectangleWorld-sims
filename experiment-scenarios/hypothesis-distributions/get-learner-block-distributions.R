rm(list = ls())
library(here)
source(here("getLearnerHypDistributions.R"))

getAlphaBlockDistributions = function(obs, block, alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5)){
  for (i in 1:length(alphas)){
    alpha = alphas[i]
    dist = getLearnerHypDistribution(obs, alpha = alpha, prior = "flat")
    save(dist, file = here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,".Rdata")))
  }
}

# tb 2
load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
obs2 = targetBlock$observations
getAlphaBlockDistributions(obs2, 2)

debugonce(getLearnerHypDistribution)

# tb 8 
load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
obs8 = targetBlock$observations
getAlphaBlockDistributions(obs8, 8)

load(here("experiment-1/data/derived/data_cartesian.Rdata"))

