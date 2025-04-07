source(here("functions/plottingFunctions.R"))
source(here("functions/generic-functions.R"))
source(here("functions/getLearnerHypDistributions.R"))
load(here("experiment-1/data/derived/all_conditions.Rdata"))

nBlocks <- 8
targetBlocks <- c(2,8)
providers <- c("helpful", "random", "misleading", "uninformative")
alphas <- c(1,0,-1,-1)
recursive <- c(F, F, F, T)

# All blocks 
for (i in 1:nBlocks) {
  for (j in 1:length(providers))   {
  provider = providers[j]
  obs = loadExperimentObs(i, 4, provider = provider)
  hyps = getLearnerHypDistribution(observations = obs, alpha = alphas[j],recursion = recursive[j],  prior = "flat", allHyp = TRUE)
  for(clue in 1:4){
    hyp <- hyps[[clue]]
    save(hyp, file = here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",provider,"-b-",i,"-c-",clue,".Rdata")))
  }
  }
  print(paste0(i, " out of ", nBlocks))
}

