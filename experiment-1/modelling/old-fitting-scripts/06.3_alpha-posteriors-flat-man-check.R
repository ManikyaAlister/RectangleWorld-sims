rm(list = ls())
##### Calculate the alpha value that best describes participant responses in a given condition #####

# setup 
library(here)
library(dplyr)
library(ggpubr)
source(here("plottingFunctions.R"))

# load experiment conditions
load(here("experiment-1/data/derived/all_conditions.Rdata"))
csConds <- c("MS","US", "RS", "HS")

all_conditions <- all_conditions %>%
  filter(conditions %in% csConds)
all_conditions <- all_conditions %>%
  arrange(clues, conditions)

# Target Block 2 (First target Block) ----------------------------------------------------------
tb2 <-all_conditions %>%
  filter(targetBlocks == 2) 

# set up empty list to fill with plots 
all_plots = list()

# Median

# load in output and plot 
for (i in 1:length(tb2[,1])){
  tb <- tb2[i,"targetBlocks"]
  cond <- tb2[i,"conditions"]
  clue <- tb2[i,"clues"]
  load(file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
  plot <- plotMeanPosteriorAlphas(posteriors, stat = "median", title = paste0("Cond: ",cond,", Clue: ", clue),generatingAlpha = 0, recovery = FALSE)
  all_plots[[i]] <- plot 
}

tb2_posteriors <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 32/4)
ggsave(plot = tb2_posteriors, filename = here("experiment-1/modelling/05_plots/tb2_posteriors_median-flat-mc.png"), height = 10, width = 12)

# Mean

for (i in 1:length(tb2[,1])){
  tb <- tb2[i,"targetBlocks"]
  cond <- tb2[i,"conditions"]
  clue <- tb2[i,"clues"]
  load(file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
  plot <- plotMeanPosteriorAlphas(posteriors, title = paste0("Cond: ",cond,", Clue: ", clue),generatingAlpha = 0, recovery = FALSE)
  all_plots[[i]] <- plot 
}

tb2_posteriors <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 32/4)
ggsave(plot = tb2_posteriors, filename = here("experiment-1/modelling/05_plots/tb2_posteriors_mean-flat-mc.png"), height = 10, width = 12)


# Target Block 8 (Second target Block) ----------------------------------------------------------
tb8 <-all_conditions %>%
  filter(targetBlocks == 8) 

# set up empty list to fill with plots 
all_plots = list()

# median

# load in output and plot 
for (i in 1:length(tb8[,1])){
  tb <- tb8[i,"targetBlocks"]
  cond <- tb8[i,"conditions"]
  clue <- tb8[i,"clues"]
  load(file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
  plot <- plotMeanPosteriorAlphas(posteriors, stat = "median", title = paste0("Cond: ",cond,", Clue: ", clue),generatingAlpha = 0, recovery = FALSE)
  all_plots[[i]] <- plot 
}

tb8_posteriors <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 32/4)
ggsave(plot = tb8_posteriors, filename = here("experiment-1/modelling/05_plots/tb8_posteriors_median-flat-mc.png"), height = 10, width = 12)

# Mean

# load in output and plot 
for (i in 1:length(tb8[,1])){
  tb <- tb8[i,"targetBlocks"]
  cond <- tb8[i,"conditions"]
  clue <- tb8[i,"clues"]
  load(file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
  plot <- plotMeanPosteriorAlphas(posteriors, title = paste0("Cond: ",cond,", Clue: ", clue),generatingAlpha = 0, recovery = FALSE)
  all_plots[[i]] <- plot 
}

tb8_posteriors <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 32/4)
ggsave(plot = tb8_posteriors, filename = here("experiment-1/modelling/05_plots/tb8_posteriors_mean-flat-mc.png"), height = 10, width = 12)

# Sum

# load in output and plot 
for (i in 1:length(tb8[,1])){
  tb <- tb8[i,"targetBlocks"]
  cond <- tb8[i,"conditions"]
  clue <- tb8[i,"clues"]
  load(file = here(paste0("experiment-1/modelling/04_output/",cond,"-clue",clue,"-tb",tb,"-post-flat-mc.Rdata")))
  plot <- plotMeanPosteriorAlphas(posteriors, title = paste0("Cond: ",cond,", Clue: ", clue),generatingAlpha = 0, recovery = FALSE, stat = "sum")
  all_plots[[i]] <- plot 
}

tb8_posteriors <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 32/4)
ggsave(plot = tb8_posteriors, filename = here("experiment-1/modelling/05_plots/tb8_posteriors_sum-flat-mc.png"), height = 10, width = 12)


