rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
source(here("plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))


exp = 3
#c = 4 # clue

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
recursion_conds = c(
  helpful = FALSE,
  random = FALSE,
  misleading = FALSE,
  uninformative = TRUE
)

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
    conditions == "HN" | conditions == "HS" ~ recursion_conds["helpful"],
    conditions == "RN" | conditions == "RS" ~ recursion_conds["random"],
    conditions == "MN" |
      conditions == "MS" ~ recursion_conds["misleading"],
    conditions == "UN" |
      conditions == "US" ~ recursion_conds["uninformative"]
  ),
  provider = case_when(
    conditions == "HS" | conditions == "HN" ~ "helpful",
    conditions == "MS" | conditions == "MN" ~ "misleading",
    conditions == "UN" | conditions == "US" ~ "uninformative",
    conditions == "RS" | conditions == "RN" ~ "random"
  )
)


# re order for plotting
all_conditions <- all_conditions[order(as.character(all_conditions$conditions)), ]



blocks <- c(5,6,7,8)
target_blocks <- c(2,8)
clues <- 1:4
providers <- c("helpful", "random", "misleading", "uninformative")

for (b in blocks) {
  for (c in clues) {
    
    print(paste0("block ", b))
    print(paste0("clue ", c))
    # All conditions have the same provider in target blocks
    if (b %in% target_blocks) {
      provider <- FALSE
    }
    
    # filter relevant conditions
    all_conditions_tmp <- all_conditions %>%
      filter(blocks == b & clues == c)
    
    plot_list = NULL
    for (i in 1:length(all_conditions_tmp[, 1])) {
      condition <- all_conditions_tmp[i, "conditions"]
      alpha <- all_conditions_tmp[i, "recovery_alpha"]
      recursion <- all_conditions_tmp[i, "recursion"]
      provider <- all_conditions_tmp[i, "provider"]
      
      
      
      if (b %in% target_blocks){
        # load posteriors 
        if (recursion) {
          # load posteriors for participant data
          load(here(
            paste0(
              "experiment-",
              exp,
              "/modelling/04_output/tb",b,"-all-alpha-posteriors-recursive.Rdata"
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
              "/modelling/04_output/tb",b,"-all-alpha-posteriors.Rdata"
            )
          ))
          # load posteriors for recovery
          load(here(paste0(
            "recovery2/data/a",alpha,"_n100_c",c,"_pr-flat.RData"
          )))
        }
      }  else {
        if (recursion) {
          # load posteriors for participant data
          load(here(paste0("experiment-3/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,"-recursive.Rdata")))
          # load posteriors for recovery
          load(here(
            paste0(
              "recovery2/data/a",alpha,"_n100_c",c,"_pr-flat_b_",b,"_",provider,"_recursion.RData"
            )))
          
        } else {
          # load posteriors for participant data
          load(here(paste0("experiment-3/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,".Rdata")))
          # load posteriors for recovery
          load(here(paste0(
            "recovery2/data/a",alpha,"_n100_c",c,"_pr-flat_b_",b,"_",provider,"_.RData"
          )))
        }
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
      
      if(b %in% target_blocks){
        ggsave(here(paste0("experiment-3/modelling/05_plots/posteriors/posteriors-",condition,"-c-", c, "-b-",b,".png")), width = 7, height = 5, plot = plot)
      } else {
        ggsave(here(paste0("experiment-3/modelling/05_plots/posteriors/posteriors-",condition,"-c-", c, "-b-",b,"-",provider,".png")), width = 7, height = 5, plot = plot)
      }
      # save individual plots 
      
      # save to list so they can be plotted together 
      plot_list[[i]] <- plot
    }
    # re-order the list so plots of the same condition are next to each other
    #plot_list <- plot_list[c(2:length(plot_list), 1)]
    
    #labels = c() # to fill in with condition names
    combined_plot = ggpubr::ggarrange(plotlist =  plot_list,
                                      ncol = 2,
                                      nrow = 4)
    combined_plot
    
    if(b %in% target_blocks){
      ggsave(here(paste0("experiment-3/modelling/05_plots/posteriors/posteriors-all-conditions-c-",c,"-b-",b,".png")), width = 8, height = 10, plot = combined_plot)
    } else {
      ggsave(here(paste0("experiment-3/modelling/05_plots/posteriors/posteriors-all-conditions-c-", c, "-b-",b,".png")), width = 8, height = 10, plot = combined_plot)
    }
    
  }
}


