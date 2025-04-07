###### Frequency of rectangles by size #########

# setup 
library(here)
source(here("functions/getLearnerHypDistributions.R"))
source(here("functions/calculatingFunctions.R"))
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

# Target Block 2 ----------------------------------------------------------

# load data
load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
observations = targetBlock$observations

alphas <- c(-1,0,1)

# Predicted distributions for each alpha 
all_dists = NULL
for (i in 1:length(alphas)){
  dist <- getLearnerHypDistribution(observations, alpha = alphas[i])
  # convert to df (probably a better way than this)
  distDf <- NULL 
  for (j in 1:4){
    distBlock <- dist[[j]]
    distDf <- rbind(distDf, distBlock)
  }
  all_dists <- rbind(all_dists,distDf)
  }

# plot
all_dists %>%
  ggplot()+
  geom_line(aes(x = size, y = posterior, colour = as.factor(alpha)))+
  theme_classic()+
  facet_grid(~clue)


# Participant data  -------------------------------------------------------

# function to create a histogram of chosen rectangles by size
sizeHist = function(data){
  data %>%
    mutate(index = as.character(index)) %>%
    group_by(cond, size_resp) %>%
    arrange(size_resp) %>%
    ggplot()+
    geom_bar(aes(x = as.factor(size_resp)))+
    geom_vline(xintercept = "228", colour = "red")+
    ylim(c(0,70))+
    facet_wrap(~clue+cond, ncol = 4, scales = "free")
}

sizeProportion = function(data){
    mutate(index = as.character(index),) %>%
    group_by(cond, size_resp) %>%
    summarise(proporion = count(index)/length(unique()))
    arrange(size_resp) %>%
    ggplot()+
    geom_bar(aes(x = as.factor(size_resp)))+
    geom_vline(xintercept = "228", colour = "red")+
    facet_wrap(~clue+cond, ncol = 4, scales = "free")
}

# Order the conditions variable for better plottinh
d_cartesian <- d_cartesian %>% 
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

tb2 <- d_cartesian %>%
  filter(block == 2)
tb8 <- d_cartesian %>%
  filter(block == 8)
tb8mc <- d_cartesian %>%
  filter(block == 8 & man_check == TRUE)
tb8mcCs <- d_cartesian %>%
  filter(block == 8 & man_check == TRUE & cond %in% c("MS", "US", "RS", "HS"))



sizeHistTb2 <- sizeHist(tb2)
ggsave(filename = "experiment-1/modelling/05_plots/size-hist-tb2-sf.png", plot = sizeHistTb2, height = 15, width = 15)

sizeHistTb8 <- sizeHist(tb8)
ggsave(filename = "experiment-1/modelling/05_plots/size-hist-tb8-sf.png", plot = sizeHistTb8, height = 15, width = 15)

sizeHistTb8Mc <- sizeHist(tb8mc)
ggsave(filename = "experiment-1/modelling/05_plots/size-hist-tb8-mc-sf.png", plot = sizeHistTb8Mc, height = 15, width = 15)

sizeHistTb8McCs <- sizeHist(tb8mcCs)
ggsave(filename = "experiment-1/modelling/05_plots/size-hist-tb8-mc-cover-sf.png", plot = sizeHistTb8McCs, height = 15, width = 15)



tb2 %>%
  mutate(index = as.character(index)) %>%
  group_by(cond, size_resp, clue) %>%
  arrange(size_resp) %>%
  count() %>%
  ggplot()+
  geom_bar(aes(x = as.factor(size_resp)))+
  #geom_col(aes(x = as.factor(size_resp), y = n, fill = cond,), position = "dodge")+
  #geom_line(aes(x = as.factor(size_resp), y = n, colour = cond, group = cond))+
  #geom_vline(xintercept = "228", colour = "red")+
  xlab("size")+
  facet_wrap(~cond)
  

# Participant data 
tb8 <- d_cartesian %>%
  filter(block == 8 & clue == 2)

tb8 %>%
  mutate(index = as.character(index)) %>%
  group_by(cond, size_resp) %>%
  arrange(size_resp) %>%
  ggplot()+
  geom_bar(aes(x = as.factor(size_resp)))+
  geom_vline(xintercept = "228", colour = "red")+
  facet_wrap(~cond, ncol = 4)


# Only participants who passed manipulation check -------------------------

tb2 <- d_cartesian %>%
  filter(block == 2 & clue == 2 & man_check == TRUE)

tb2 %>%
  mutate(index = as.character(index)) %>%
  group_by(cond, size_resp) %>%
  arrange(size_resp) %>%
  ggplot()+
  geom_bar(aes(x = as.factor(size_resp)))+
  geom_vline(xintercept = "228", colour = "red")+
  xlab("size")+
  facet_wrap(~cond, ncol = 4)

# Participant data 
tb8 <- d_cartesian %>%
  filter(block == 8 & clue == 2 & man_check == TRUE)

tb8 %>%
  mutate(index = as.character(index)) %>%
  group_by(cond, size_resp) %>%
  arrange(size_resp) %>%
  ggplot()+
  geom_bar(aes(x = as.factor(size_resp)))+
  geom_vline(xintercept = "228", colour = "red")+
  facet_wrap(~cond, ncol = 4)

# line graphs collapsed across clue 

tb2 <- d_cartesian %>%
  filter(block == 2)

tb2 %>%
  mutate(index = as.character(index)) %>%
  group_by(cond, size_resp, clue) %>%
  arrange(size_resp) %>%
  count() %>%
  ggplot()+
  #geom_col(aes(x = as.factor(size_resp), y = n, fill = cond,), position = "dodge")+
  geom_line(aes(x = as.factor(size_resp), y = n, colour = cond, group = cond))+
  #geom_vline(xintercept = "228", colour = "red")+
  xlab("size")+
  facet_wrap(~clue, scales = "free")

