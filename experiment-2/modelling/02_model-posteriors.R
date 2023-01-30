rm(list = ls())
library(here)
library(dplyr)

#' Get the probability that different alphas would have generated a given response
#'
#' @param data Participant data filtered by block
#' @param block Experiment block that you are examininig (e.g., target block 2 or 8)
#' @param alphas Vector of alphas to 
#'
#' @return
#' @export
#'
#' @examples
fitAlphas = function(data, block, alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5)) {
  all_alpha_posteriors = NULL
  # loop through all alphas
  for (j in 1:length(alphas)) {
    alpha = alphas[j]
    all_posteriors = NULL
    # loop through each clue
    for (i in 1:length(data[, 1])) {
      block = data[i, "block"]
      clue = data[i, "clue"]
      cond = data[i, "cond"]
      # load pre-calculated probability distributions for the given experiment block and alpha. Make sure 
      # that the block has been pre-calculated or the code will break. 
      load(here(
        paste0(
          "experiment-scenarios/hypothesis-distributions/b-",
          block,
          "-dist-alpha_",
          alpha,
          ".Rdata"
        )
      ))
      # get the observations corresponding to the clue number
      resp_dist = dist[[clue]]
      # take the posterior of participant's rectangle
      posterior = resp_dist[resp_dist[, "index"] == data[i, "index"], ]
      posterior = select(posterior,-prior)
      # include whether respondent passed manipulation check
      man_check = data[i,"man_check"]
      posterior = cbind(posterior, cond, man_check)
      all_posteriors = rbind(all_posteriors, posterior)
    }
    all_alpha_posteriors = rbind(all_alpha_posteriors, all_posteriors)
    print(paste0(j, " out of ", length(alphas)))
  }
  all_alpha_posteriors
}

# load data
load(here("experiment-2/data/derived/data_cartesian.Rdata"))

# target block 2
tb2  = filter(d_cartesian, block == 2)
all_alpha_posteriors = fitAlphas(block = 2, data = tb2)
save(all_alpha_posteriors, file = here("experiment-2/modelling/04_output/tb2-all-alpha-posteriors.Rdata"))

# target block 8
tb8  = filter(d_cartesian, block == 8)
all_alpha_posteriors = fitAlphas(block = 8, data = tb8)
save(all_alpha_posteriors, file = here("experiment-2/modelling/04_output/tb8-all-alpha-posteriors.Rdata"))

