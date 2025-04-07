rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)
exp = 3
source(here("functions/plottingFunctions.R"))
source(here("functions/calculatingFunctions.R"))
load(here(paste0("experiment-",exp,"/data/derived/all_conditions.Rdata")))
load(here(paste0("experiment-",exp,"/data/derived/data_cartesian.Rdata")))

# adjust all_conditions so that it is agnostic to clue number.
blocks <- 8
manipulations = unique(all_conditions$conditions)
all_conditions_noclue <- expand.grid(blocks = blocks, conditions = manipulations)
clues = c(2,4)
# All blocks 
for (i in blocks) {
  plot_list = NULL
  for(j in 1:length(clues)){
    clue = clues[j]
    block_conds <- all_conditions_noclue %>%
      filter(blocks == i)
    plot <- plotHeatMapsFacet(all_conditions = block_conds, experiment = exp, clues = clue, t = paste0("Experiment ",exp,": Generalisation after seeing ",clue," clues"))
    path = as.character(paste0("experiment-",exp,"/modelling/05_plots/heatmap_facet_b",blocks,"-c-",clue,".png", collapse = ""))
    ggsave(file = here(path), width = 20, height = 12, plot = plot)
    plot_list[[j]] <- plot
  }
  ggarrange(plotlist = plot_list, nrow = 2, ncol = 1)
  
  ggsave(filename = here(paste0("experiment-",exp,"/modelling/05_plots/heatmap_facet_manuscript_b",i,"_e",exp,".png")), width = 20, height = 12)
}


