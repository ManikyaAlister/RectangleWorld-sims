rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("getLearnerHypDistributions.R"))
source(here("genericFunctions.R"))

getAlphaBlockDistributions = function(obs, block, alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5), recursion = FALSE, provider = FALSE){
  for (i in 1:length(alphas)){
    alpha = alphas[i]
    if (recursion){
      dist = getLearnerHypDistribution(obs, alpha = alpha, prior = "flat", recursion = TRUE)
      save(dist, file = ifelse(is.character(provider), 
                               here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,"-recursive-learner-",provider,".Rdata")),
                               here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,"-recursive-learner.Rdata"))
                               ))
    } else {
      dist = getLearnerHypDistribution(obs, alpha = alpha, prior = "flat")
      save(dist, file = ifelse(is.character(provider), 
                               here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,"-learner-",provider,".Rdata")),
                               here(paste0("experiment-scenarios/hypothesis-distributions/b-",block,"-dist-alpha_",alpha,"-learner.Rdata"))
      ))
    }
      print(paste0(i, " out of ", length(alphas)))
  }
}



# all non-target blocks ---------------------------------------------------

# define target blocks
tbs = c(2,8)

# Retrieve the values from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
block <- as.numeric(args[1])

# target blocks are consistent across providers, so provider is false in those blocks
ifelse(block %in% tbs, provider <- FALSE, provider <- args[2])

# print the block and provider so I know what's running
print(paste0("block: ", block, " provider: ", provider))

observations <-
  loadExperimentObs(b = block,
                    clueNum = 4, # get all clues
                    provider = provider)

# non-recursive
getAlphaBlockDistributions(obs = observations,
                           block = block,
                           provider = provider,
                           recursion = FALSE)

# recursive
getAlphaBlockDistributions(obs = observations, 
                           block = block, 
                           provider = provider,
                           recursion = TRUE)


# 
# # tb 2
# load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
# obs2 = targetBlock$observations
# 
# # non recursive 
# getAlphaBlockDistributions(obs2, 2)
# 
# # recursive learner 
# getAlphaBlockDistributions(obs2, 2, recursion = TRUE)
# 
# # tb 8 
# load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
# obs8 = targetBlock$observations
# 
# # non-recursive learner
# getAlphaBlockDistributions(obs8, 8)
# 
# # recursive learner
# getAlphaBlockDistributions(obs8, 8, recursion = TRUE)

