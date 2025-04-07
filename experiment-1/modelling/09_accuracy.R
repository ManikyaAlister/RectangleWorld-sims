rm(list =ls())
library(here)
library(tidyverse)
source(here("functions/calculatingFunctions.R"))
library(patchwork)

# load data
load(here("experiment-1/data/derived/data_cartesian.Rdata"))

# get participant guesses
guesses <- d_cartesian[,2:5]

# get ground truth
ground_truth <- d_cartesian[,grep("ground_truth_", colnames(d_cartesian))]

accuracy <- apply(cbind(guesses, ground_truth), 1, function(row) {
  rect1 <- row[1:4]
  rect2 <- row[5:8]
  rectOverlap(rect1, rect2)
})


conditions <- unique(d_cartesian$cond)
cover_story <- grep("S", conditions, value = TRUE)
no_cover_story <- grep("N", conditions, value = TRUE)

# add accuracy column into the data
d = d_cartesian %>%
  mutate(accuracy = accuracy)

save(d, file = here("experiment-1/data/derived/accuracy.Rdata"))


d_accuracy = d %>%
  group_by(cond, block, clue) %>%
  summarise(mean = mean(accuracy),
            sd = sd(accuracy),
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         cover_story = case_when(
           cond %in% cover_story ~ TRUE,
           cond %in% no_cover_story ~ FALSE
         ))

# accuracy over time 
plot_acc_cs_time <- d_accuracy %>%
  filter(cover_story)  %>%
  ggplot()+
  geom_line(aes(x = clue, y = mean, colour = cond))+
  facet_grid(~block)+
  theme_bw()

plot_acc_ns_time <- d_accuracy %>%
  filter(!cover_story)  %>%
  ggplot()+
  geom_line(aes(x = clue, y = mean, colour = cond))+
  facet_grid(~block)+
  theme_bw()

plot_acc_cs_time / plot_acc_ns_time

# Keith's accuracy method 
pts <- expand.grid(1:10, 1:10)


keith_accuracy <- apply(cbind(guesses, ground_truth), 1, function(row) {
  # figure out which points are inside the learner's guess
  rect1 <- row[1:4]
  in_rect1 <- isInRectangle(pts, rect1)
  
  # figure out which points are in the true rectangle
  rect2 <- row[5:8]
  in_rect2 <- isInRectangle(pts, rect2)
  
  # figure out which cells are correctly judged as inside or outside of the true rectangle byt the learner
  score <- sum(in_rect1 == in_rect2)
  score
})

# add accuracy column into the data
d = d_cartesian %>%
  mutate(keith_accuracy = keith_accuracy)

d_keith_accuracy = d %>%
  group_by(cond, block, clue) %>%
  summarise(mean = mean(keith_accuracy),
            sd = sd(keith_accuracy),
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         cover_story = case_when(
           cond %in% cover_story ~ TRUE,
           cond %in% no_cover_story ~ FALSE
         ))


# accuracy over time 
plot_acc_cs_time <- d_keith_accuracy %>%
  filter(cover_story)  %>%
  ggplot()+
  geom_line(aes(x = clue, y = mean, colour = cond))+
  facet_grid(~block)+
  theme_bw()

plot_acc_cs_time
