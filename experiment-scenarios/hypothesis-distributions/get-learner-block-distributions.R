rm(list = ls())
library(here)
source(here("getLearnerHypDistributions.R"))

getAlphaBlockDistributions = function(obs, block, alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5), recursion = FALSE){
  for (i in 1:length(alphas)){
    alpha = alphas[i]
    if (recursion){
      dist = getLearnerHypDistribution(obs, alpha = alpha, prior = "flat", recursion = TRUE)
      save(dist, file = here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,"-recursive-learner.Rdata")))
    } else {
      dist = getLearnerHypDistribution(obs, alpha = alpha, prior = "flat")
      save(dist, file = here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,".Rdata")))
    }
  }
}

# tb 2
load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
obs2 = targetBlock$observations

# non recursive 
getAlphaBlockDistributions(obs2, 2)

# recursive learner 
getAlphaBlockDistributions(obs2, 2, recursion = TRUE)

# tb 8 
load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
obs8 = targetBlock$observations

# non-recursive learner
getAlphaBlockDistributions(obs8, 8)

# recursive learner
getAlphaBlockDistributions(obs8, 8, recursion = TRUE)
