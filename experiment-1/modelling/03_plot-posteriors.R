rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
source(here("plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))
exp = 1
c =4# clue

# function for wrangling plot data 
getPosteriorPlotData = function(posteriors){
  plot_data <- posteriors %>%
    mutate(alpha = as.factor(alpha)) %>%
    group_by(alpha) %>%
    summarise(
      mean = mean(posterior),
      median = median(posterior),
      sum = sum(posterior)
    ) %>%
    # normalize so that everything is on the same scale
    mutate(
      mean = mean / sum(mean),
      median = median / sum(median),
      sum = sum / sum(sum)
    )
  plot_data
}


# Define alphas for each condition for recovery comparison
alphas = c(
  helpful = 1,
  random = 0,
  misleading = -1,
  uninformative = -1
)

# Define whether each condition is recursive
recursion = c(
  helpful = FALSE,
  random = FALSE,
  misleading = FALSE,
  uninformative = TRUE
)

# Target block 8 ----------------------------------------------------------
all_conditions <- all_conditions %>%
  filter(blocks == 8 & clues == c)


# Define recovery alpha and recursion for each condition
all_conditions <- all_conditions %>% mutate(
  recovery_alpha = case_when(
    conditions == "HN" | conditions == "HS" ~ alphas["helpful"],
    conditions == "RN" | conditions == "RS" ~ alphas["random"],
    conditions == "MN" | conditions == "MS" ~ alphas["misleading"],
    conditions == "UN" |
      conditions == "US" ~ alphas["uninformative"]
  ),
  recursion = case_when(
    conditions == "HN" | conditions == "HS" ~ recursion["helpful"],
    conditions == "RN" | conditions == "RS" ~ recursion["random"],
    conditions == "MN" |
      conditions == "MS" ~ recursion["misleading"],
    conditions == "UN" |
      conditions == "US" ~ recursion["uninformative"]
  )
)

# re order for plotting
all_conditions <- all_conditions[order(as.character(all_conditions$conditions)), ]


plot_list = NULL
for (i in 1:length(all_conditions[, 1])) {
  condition <- all_conditions[i, "conditions"]
  alpha <- all_conditions[i, "recovery_alpha"]
  recursion <- all_conditions[i, "recursion"]
  
  if (recursion) {
    # load posteriors for participant data
    load(here(
      paste0(
        "experiment-",
        exp,
        "/modelling/04_output/tb8-all-alpha-posteriors-recursive.Rdata"
      )
    ))
    # load posteriors for recovery
    load(here(
      paste0("recovery2/data/a",alpha,"_n100_c",c,"_pr-flat_recursion.RData")
    ))
    
  } else {
    # load posteriors for participant data
    load(here(
      paste0(
        "experiment-",
        exp,
        "/modelling/04_output/tb8-all-alpha-posteriors.Rdata"
      )
    ))
    # load posteriors for recovery
    load(here(paste0(
      "recovery2/data/a",alpha,"_n100_c",c,"_pr-flat.RData"
    )))
  }
  
  # wrangle recovery data for plotting
  recovery_plotting <- getPosteriorPlotData(posteriors)
  
  # wrangle participant data for plotting
  all_data <- all_alpha_posteriors %>%
    filter(cond == condition, clue == c) %>%
    mutate(alpha = as.factor(alpha))
  sum <- getPosteriorPlotData(all_data)
  
  plot <-
    plotPosteriors(p_data = sum,
                   statistic = "median",
                   recovery_data = recovery_plotting,
                   subtitle = condition)
  
  # save individual plots 
  ggsave(here(paste0("experiment-1/modelling/05_plots/posteriors-",condition,"-c-", c, ".png")), width = 7, height = 5, plot = plot)
  
  # save to list so they can be plotted together 
  plot_list[[i]] <- plot
}
# re-order the list so plots of the same condition are next to each other
#plot_list <- plot_list[c(2:length(plot_list), 1)]

labels = c() # to fill in with condition names
combined_plot = ggpubr::ggarrange(plotlist =  plot_list,
                  ncol = 2,
                  nrow = 4)
combined_plot

ggsave(here(paste0("experiment-1/modelling/05_plots/posteriors-all-conditions-c-", c, ".png")), plot = combined_plot)
