rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)

source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
load(here("experiment-2/data/derived/all_conditions.Rdata"))
load(here("experiment-2/data/derived/data_cartesian.Rdata"))

applyIsInRect <- function(df, rectangle) {
  inRectangle <- apply(df[,c("x","y")], 1, function(p) isInRectangle(p, rectangle))
  return(inRectangle)
}



# plotHeatMaps = function(d, all_conditions){
#   # Set up grid of all possible points
#   x  = seq(0.5, 9.5)
#   y = seq(0.5, 9.5)
#   pts = expand.grid(x,y)
#   colnames(pts) = c("x","y")
#   
#   # Find out how many conditions there are
#   nConds = length(all_conditions[,1])
#   
#   # Empty list to fill with plots for arrange
#   plotList = list()
#   
#   # Loop through each condition, creating a seperate heat map for each
#   for (condId in 1:nConds){
#     # Get condition data (block, conditions, clue number)
#     b <- all_conditions[condId,"targetBlocks"]
#     condition <- all_conditions[condId,"conditions"]
#     clueNum <- all_conditions[condId,"clues"]
#     
#     # Filter data based on those conditions
#     data <- d %>%
#       filter(cond == condition & clue == clueNum & block == b) 
#     
#     # Find out how many participant responses there are
#     nResp <- length(data[,1])
#     
#     # Empty data frame to fill with the points contained with a participant response
#     ptsIn <- NULL
#     
#     # Loop through each participant response, seeing which grid cells/points were contained within each response
#     for (j in 1:nResp) {
#       rect <- c(data[j,"x1"], data[j,"y1"], data[j,"x2"], data[j,"y2"])
#       isIn <- applyIsInRect(pts, rect)
#       ptsIn <- cbind(ptsIn, isIn)
#     }
#     # Calculate how many times a point was contained within a given response
#     sums <- rowSums(ptsIn)
#     
#     # Convert to probability
#     probs <- sums/sum(sums)
#     
#     # Combine into a single data frame
#     ptProbs  <- cbind(pts, probs) 
#     
#     # Save data
#     save(ptProbs, file = here(paste0("experiment-2/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
#     
#     # Load clues pertaining to condition
#     if(b == 2){
#       load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
#     } else if(b == 8){
#       load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
#     }
#     
#     # Rename columns for plot legend
#     ptProbs <- rename(ptProbs, Probability = probs)
#   
#     
#     # Get observations pertaining to condtition
#     obs <- targetBlock$observations[1:clueNum,]
#     
#     if(condition == "HS"){
#       fullCond <- "Helpful, Cover Story"
#     } else if (condition == "HN") {
#       fullCond <- "Helpful, No Cover Story"
#     } else if(condition == "MS"){
#       fullCond <- "Misleading, Cover Story"
#     } else if (condition == "MN") {
#       fullCond <- "Misleading, No Cover Story"
#     } else if(condition == "US"){
#       fullCond <- "Uninformative, Cover Story"
#     } else if (condition == "UN") {
#       fullCond <- "Uninformative, No Cover Story"
#     } else if(condition == "RS"){
#       fullCond <- "Random, Cover Story"
#     } else if (condition == "RN") {
#       fullCond <- "Helpful, No Cover Story"}
#     
#     st = fullCond
#     
#     # Plot heat map for that condition
#     heatMap <- ptProbs %>% ggplot() +
#       geom_raster(aes(x = ptProbs[,1], y = ptProbs[,2], fill = Probability))+
#       scale_fill_gradientn(colours = c("white","lightpink", "hotpink","lightblue","blue","navyblue"))+
#       geom_rect(aes(xmin = data[1,"ground_truth_x1"], ymin = data[1,"ground_truth_y1"], xmax = data[1,"ground_truth_x2"], ymax = data[1,"ground_truth_y2"]), alpha = 0, colour = "red", linetype = 4, linewidth = 0.2)+
#       geom_point(data = obs, aes(x = x, y = y, colour = category))+
#       scale_colour_manual(values = c("positive" = "darkgreen", "negative" = "red"))+
#       {if ( clueNum == 1)labs(subtitle = st)} +
#       guides(color = FALSE)+
#       theme_void()+
#       theme(axis.text.x=element_blank(),
#             axis.ticks.x=element_blank(),
#             axis.text.y=element_blank(),
#             axis.ticks.y=element_blank(),
#             text = element_text(size = 4),
#             plot.margin = margin(0, 0, 0, 0, "cm"))+
#       labs(x = "", y = "")
#     ggsave(filename = here(paste0("experiment-2/modelling/05_plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")))
#     
#     # Save plot to list 
#     plotList[[condId]] <- heatMap
#     # track progress in console
#     print(condId)
#   }
#   # Create and save plot 
#   ggarrange(plotlist = plotList, common.legend = TRUE, ncol = 4, nrow = 8, widths = c(1,1, 1,1), heights = c(2,2,2,2), legend = c("bottom","left"))
#   ggsave(filename = here(paste0("experiment-2/modelling/05_plots/heatmap-all-b",b,".png")))
# }

tb2 <- all_conditions %>% filter(targetBlocks == 2)
plotHeatMaps(d = d_cartesian, all_conditions = tb2, experiment = 2)

tb8 <- all_conditions %>% filter(targetBlocks == 8)
plotHeatMaps(d = d_cartesian, all_conditions = tb8, experiment = 2)


