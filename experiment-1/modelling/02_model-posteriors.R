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
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb2-all-alpha-posteriors.Rdata"))

# target block 8
tb8  = filter(d_cartesian, block == 8)
all_alpha_posteriors = fitAlphas(block = 8, data = tb8)
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb8-all-alpha-posteriors.Rdata"))

# all blocks 
blocks <- c(1,3,4,5,6,7)

for (i in 1:length(blocks)){
  block <- blocks[i]
  d <- filter(d_cartesian, block == block)
  all_alpha_posteriors <- fitAlphas(block = block, data = d)
  save(all_alpha_posteriors, file = here(paste0("experiment-1/modelling/04_output/b",block,"-all-alpha-posteriors.Rdata")))
}

# Recursive alphas --------------------------------------------------------

# target block 2
all_alpha_posteriors = fitAlphas(block = 2, data = tb2, recursion = TRUE)
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb2-all-alpha-posteriors-recursive.Rdata"))

# target block 8
all_alpha_posteriors = fitAlphas(block = 8, data = tb8, recursion = TRUE)
save(all_alpha_posteriors, file = here("experiment-1/modelling/04_output/tb8-all-alpha-posteriors-recursive.Rdata"))