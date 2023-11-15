rm(list = ls())
library(here)
library(dplyr)
source(here("getLearnerHypDistributions.R"))
# load data
load(here("experiment-1/data/derived/data_cartesian.Rdata"))


# Non-recursive alphas ----------------------------------------------------

# target block 2
tb2  = filter(d_cartesian, block == 2)
all_alpha_posteriors = fitAlphas(block = 2, data = tb2)
all_alpha_posteriors$block <- 2
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb2-all-alpha-posteriors.Rdata"))

print("Target block 2 finished")


# target block 8
tb8  = filter(d_cartesian, block == 8)
all_alpha_posteriors = fitAlphas(block = 8, data = tb8)
all_alpha_posteriors$block <- 8
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb8-all-alpha-posteriors.Rdata"))

print("Non recursive target blocks finished")

# all blocks 
blocks <- c(1,3,4,5,6,7)
providers <- c("helpful","random", "misleading", "uninformative")


for (i in 1:length(blocks)){
  for (j in 1:length(providers)){
    
    b <- blocks[i]
    provider <-  providers[j]
    print(b)
    print(provider)
    # get condition participants were in
    if (provider == "helpful"){
      condition <- c("HS", "HN") 
    } else if (provider == "random") {
      condition <- c("RS", "RN")
    } else if (provider == "misleading"){
      condition <- c("MS", "MN") 
    } else if (provider == "uninformative"){
      condition <- c("US", "UN")
    }
    
    d <- d_cartesian %>%
      filter(block == b, cond %in% condition)
    
    # if(b == 5 & provider == "helpful"){
    #   d$obs_index <- 1:nrow(d)
    #   d <- d[d$obs_index != 228,]  # <- on this trial, the participant made an ineligible guess. Idk how. 
    # }
    
    all_alpha_posteriors <- fitAlphas(block = b, data = d, provider = provider)
    all_alpha_posteriors$block <- b
    save(all_alpha_posteriors, file = here(paste0("experiment-1/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,".Rdata")))
  }
}

print("non-recursive blocks finished")


# Recursive alphas --------------------------------------------------------

# target block 2
all_alpha_posteriors = fitAlphas(block = 2, data = tb2, recursion = TRUE)
all_alpha_posteriors$block <- 2
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb2-all-alpha-posteriors-recursive.Rdata"))

print("Target block 2 finished")

# target block 8
all_alpha_posteriors = fitAlphas(block = 8, data = tb8, recursion = TRUE)
all_alpha_posteriors$block <- 8
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb8-all-alpha-posteriors-recursive.Rdata"))

print("Recursive target blocks finished")


# recursive
for (i in 1:length(blocks)){
  for (j in 1:length(providers)){
    
    b <- blocks[i]
    provider <-  providers[j]
    print(provider)
    # get condition participants were in
    if (provider == "helpful"){
      condition <- c("HS", "HN") 
    } else if (provider == "random") {
      condition <- c("RS", "RN")
    } else if (provider == "misleading"){
      condition <- c("MS", "MN") 
    } else if (provider == "uninformative"){
      condition <- c("US", "UN")
    }
    
    d <- d_cartesian %>%
      filter(block == b, cond %in% condition)
    
    if(b == 5 & provider == "helpful"){
      d$obs_index <- 1:nrow(d)
      d <- d[d$obs_index != 228,]  # <- on this trial, the participant made an ineligible guess. Idk how. 
    }
    
    all_alpha_posteriors <- fitAlphas(block = b, data = d, provider = provider, recursion = TRUE)
    all_alpha_posteriors$block <- b
    save(all_alpha_posteriors, file = here(paste0("experiment-1/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,"-recursive.Rdata")))
  }
}
