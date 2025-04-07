##### Script to see the responses of a single participant ######
rm(list = ls())
# set up
library(here)
library(dplyr)
source(here("functions/plottingFunctions.R"))

# load data 
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

# all participant pids
all_pids <- unique(d_cartesian$pid)

# 1. Run first: 
# Function that choose a random participant in a given condition and returns a data frame of their responses 
sampleParticipant = function(condition, data){
  d_cond <- data %>%
    filter(cond == condition)
  pids <- unique(d_cond$pid)
  p <- sample(pids, size = 1)
  d_p <- d_cond %>%
    filter(pid == p)
  d_p
}


# 2. Run second: previous output entered as data below
#' @param cond Either "helpful", "random", "misleading","uninformative". 
plotResponse = function(data, cond, nBlocks = 8){
  all_plots <- list()
  blockOrder <- c(1,2,6,7,3,8,5,10)
  trueRects <- c("2376","5296","2396", "8398", "1198", "3344", "2299", "1458")
  for (i in 1:nBlocks){
  trueRect <- trueRects[i]
  if(trueRect == "5296"){
    load("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata")
    observations = targetBlock$observations
  } else if(trueRect == "1458"){
    load("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata")
    observations = targetBlock$observations
  } else {
    load(here(paste0("experiment-scenarios/hand-picked-blocks/data/",blockOrder[i],"-",trueRect,"-",cond,"/",blockOrder[i],"-",trueRect,"-",cond,"trial-obs.Rdata")))
    observations = blockData$observations %>%
      mutate(x = x-0.5,
             y = 10-(y-0.5),
             category = observed)
  }
  observations$order = 1:4
  plot <- data %>% 
    filter(block == i) %>%
    ggplot() +
    geom_point(data = observations, aes(x = x, y = y, colour = category), size = 3, show.legend = FALSE)+
    scale_color_manual(values = c("positive" = "green", "negative" = "red"))+
    geom_text(data = observations, aes(label = order, x = x, y = y))+
    geom_rect(aes(xmin = x1, ymin = y1, xmax = x2, ymax = y2), colour = "blue", alpha = 0)+
    geom_rect(aes(xmin = ground_truth_x1, ymin = ground_truth_y1, xmax = ground_truth_x2, ymax = ground_truth_y2), fill = "yellow", alpha = 0.4)+
    facet_wrap(~clue, ncol = 2)
  all_plots[[i]] <- plot
  }
  expCond <- unique(data$cond)
  pid <- unique(data$pid)
  fullPlot <- ggpubr::ggarrange(plotlist = all_plots)
  annotate_figure(fullPlot, top = text_grob(paste0("participant: ",pid, " cond: ", expCond), 
                                        color = "black", face = "bold", size = 14))
  ggsave(filename = here(paste0("experiment-1/subject-level-analysis/02_figures/",expCond,"-",pid,".png")))
}

# example
tm = sampleParticipant("US", d_cartesian)
plotResponse(tm, "uninformative")
