rm(list = ls())
library(here)

load(here("experiment-3/data/derived/data_priors_cartesian.Rdata"))
source(here("plottingFunctions.R"))
filter_priors = function(data, block) {
  b <- block
  prior <- data %>%
    filter(block %in% b) %>%
    select(x1, y1, x2, y2, size_resp, index)
  prior
}

prior_b8 <- filter_priors(d_priors_cartesian, 8)
prior_all <- filter_priors(d_priors_cartesian, 1:8)

# get the full spectrum of possible hypotheses
load(here("datafiles/x0to10y0to10.RData"))


prior_hist = function(data, hyp) {
  
  # rename column for consistency with data
  hyp$size_resp <- as.character(hyp$size)
  
  hyp %>%
    ggplot(aes(x = size_resp)) +
    geom_bar(data = data) 
}


# plot priors

# histogram of prior size
prior_hist(prior_b8, hyp)
prior_hist(prior_all, hyp)


# rectangles overlayed on each other
plotHypotheses(r = prior_b8[,1:4],title = "", subtitle = "Priors for block 8")
plotHypotheses(r = prior_all[,1:4],title = "", subtitle = "Priors for all blocks")

