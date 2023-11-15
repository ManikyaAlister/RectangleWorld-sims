rm(list =ls())
library(here)
source(here("calculatingFunctions.R"))
library(patchwork)

# load data
load(here("experiment-2/data/derived/data_cartesian.Rdata"))

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
save(d, file = here("experiment-2/data/derived/accuracy.Rdata"))

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
