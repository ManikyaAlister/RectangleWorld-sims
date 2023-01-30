###### Frequency of rectangles by size #########

# setup 
library(here)
source(here("getLearnerHypDistributions.R"))
source(here("calculatingFunctions.R"))
source(here("plottingFunctions.R"))

# load generic data
load(here("experiment-2/data/derived/all_conditions.Rdata"))
source(here("getLearnerHypDistributions.R"))
load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
observations = targetBlock$observations

# load participant data
load(here("experiment-2/data/derived/data_cartesian.Rdata"))

# get all sizes of rectangles that could be drawn for the 4th clue
sizes <- unique(d_cartesian$size_resp)
sizes <- sizes[sizes >= 16] # 16 is the smallest size
sizes <- sizes[order(sizes)] # sort ascending 
dput(sizes) # let's me paste directly into function (lazy, I know, but under need to do quickly)

# Model predictions -------------------------------------------------------

alphas <- c(-1,0,1)
clues <- c(4)
# Predicted distributions for each alpha 
all_dists = NULL
for (i in 1:length(alphas)){
  dist <- getLearnerHypDistribution(observations, alpha = alphas[i], prior = "flat")
  # convert to df (probably a better way than this)
  distDf <- NULL 
  for (j in clues){
    distBlock <- dist[[clues]]
    distDf <- rbind(distDf, distBlock)
  }
  all_dists <- rbind(all_dists,distDf)
}

# get probability distributions
all_dists <- all_dists %>%
  mutate(Alpha = as.factor(alpha)) %>%
  mutate(clue_name = case_when(
    clue == 1 ~ "1st Clue",
    clue == 4 ~ "4th Clue"
  ))


# plot
modelPredPlot <- all_dists %>%
  mutate(Alpha = as.factor(alpha)) %>%
  mutate(clue_name = case_when(
    clue == 1 ~ "1st Clue",
    clue == 4 ~ "4th Clue"
  ))%>%
  ggplot()+
  geom_line(aes(x = size, y = posterior, colour = Alpha))+
  scale_colour_manual(values = c("1" = "darkgreen", "0" = "lightblue", "-1" = "darkred"))+
  theme_classic()+
  ylim(c(0,0.05))+
  theme(
    legend.position = c(0.9, 0.8), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key.height = unit(0.25,"cm"),
    text = element_text(size = 6)
  )+
  labs(x = "Rectangle Size (increasing)", y = "Posterior", subtitle = "Model Predictions (4th Clue)")
#facet_grid(~clue_name, scales = "free")


# Get all possible sizes of rectangles for x axis scaling 


# Participant responses ---------------------------------------------------

sizeHist = function(data){
  # Vector of all possible sizes after the 4th clues 
  size <- c(16, 18, 20, 21, 24, 25, 27, 28, 30, 32, 35, 36, 40, 42, 45, 
            48, 49, 50, 54, 56, 60, 63, 64, 70, 72, 80, 81, 90, 100)
  data %>%
    mutate(index = as.character(index)) %>%
    group_by(cond, size_resp) %>%
    arrange(size_resp) %>%
    ggplot()+
    labs(x = "Rectangle Size (Increasing)")+
    geom_bar(aes(x = as.factor(size_resp), fill = cond), show.legend = FALSE)+
    scale_fill_manual(values = c("HS" = "darkgreen", "HN" = "darkgreen", "RS" = "lightblue", "RN" = "lightblue", "MS" = "darkred", "MN" = "darkred", "UN" = "orange", "US" = "orange"))+
    geom_vline(xintercept = "228", colour = "red")+
    scale_x_discrete(sizes)+
    #geom_line(data = all_dists, aes(x = size, y = posterior, colour = Alpha))+
    scale_colour_manual(values = c("1" = "darkgreen", "0" = "lightblue", "-1" = "darkred"))+
    #scale_y_continuous(sec.axis = sec_axis(~.*0.0005, name="Posterior")) +
    theme_classic()+
    ylim(c(0,100))+
    theme(#axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks.y=element_blank(),
      text = element_text(size = 4),
      strip.background = element_blank(),
      #plot.margin = margin(-1, 0, -1, 0, "cm"),
      strip.text = element_text(margin = margin(0,0,0,0, "cm"), size = 5))
    #facet_wrap(~cond+Experiment, ncol = 2)
}

# different plot for each condition (only interested in the 4th clue and 8th block)
all_conditions_4_8 <- all_conditions %>%
  filter(clues == 4 & targetBlocks == 8)

for (i in 1:length(all_conditions_4_8[,1])){
 condition <- all_conditions_4_8[i,"conditions"]
 data <- filter(d_cartesian) %>%
   filter(block == 8 & clue == 4 & cond == condition)
 sizeHist(data)
 ggsave(filename = here(paste0("experiment-2/modelling/05_plots/size_hist_b8_c4_",condition,".png")), width = 2, height = 1)
}


