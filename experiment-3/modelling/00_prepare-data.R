library(here)
library(dplyr)
source(here("genericFunctions.R"))

# Convert clean experiment data into format that can be used for modelling and analyses. 

# load pilot data 
load(here("experiment-3/data/clean/clean_data_turk_pilot.rdata"))
pilot_data <- data
pilot_subjs <- unique(pilot_data$pid)

# load main participant data 
load(here("experiment-3/data/clean/clean_data.rdata"))
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
load(here("experiment-scenarios/target-blocks/data/target-trial-2-Cartesian.Rdata"))

# grid size 
H <- 10
# Convert participant responses and ground truths from grid world to Cartesian world
# all participant responses are - 0.5
# - 1 to x1 and y2 of ground truth rectangles
# all y coordinates are 10 - y
d_cartesian <- data %>%
  filter(!pid %in% pilot_subjs) %>% # make sure pilot participants are not included
  mutate(response_x1 = response_x1-1,
         response_y1 = H-response_y1,
         # no adjustment need for response x2
         response_y2 = H-(response_y2-1),
         ground_truth_x1 = ground_truth_x1-1,
         ground_truth_y1 = H-ground_truth_y1,
         # no adjustment needed for ground truth x2
         ground_truth_y2 = H-(ground_truth_y2-1))%>%
  rename(x1 = response_x1, y1 = response_y1, x2 = response_x2, y2 = response_y2)

# Rectangle indexes necessary for modelling functions
indexes = getRectangleIndex(d_cartesian, nRectangles = length(d_cartesian[,1]))
d_cartesian$index = indexes

# rectangle sizes 
d_cartesian$size_resp = apply(d_cartesian[,c("x1", "y1", "x2", "y2")], 1, findSize)
d_cartesian$size_truth = apply(d_cartesian[,c("ground_truth_x1", "ground_truth_y1", "ground_truth_x2", "ground_truth_y2")], 1, findSize)

# did the participant guess taht they were in the correct condition? 
d_cartesian <- d_cartesian %>%
  mutate(man_check = case_when(
    cond == "HN" & follow_up == "r_helpful" ~ TRUE,
    cond == "RN" & follow_up == "r_random" ~ TRUE,
    cond == "UN" & follow_up == "r_uninformative" ~ TRUE,
    cond == "MN" & follow_up == "r_misleading" ~ TRUE,
    cond == "HS" & follow_up == "r_helpful" ~ TRUE,
    cond == "RS" & follow_up == "r_random" ~ TRUE,
    cond == "US" & follow_up == "r_uninformative" ~ TRUE,
    cond == "MS" & follow_up == "r_misleading" ~ TRUE,
    # okay if participants couldn;t distinguish between misleading and uninformative 
    cond == "US" & follow_up == "r_misleading" ~ TRUE,
    cond == "MS" & follow_up == "r_uninformative" ~ TRUE,
    TRUE ~ FALSE
  ))

follow_up_report <- d_cartesian %>%
  filter(block == 1 & clue == 1) %>%
  group_by(cond, follow_up) %>%
  summarise(n())

d_priors_cartesian <- d_cartesian %>%
  filter(clue == 0)

save(d_priors_cartesian, file = here("experiment-3/data/derived/data_priors_cartesian.Rdata"))



d_cartesian <- d_cartesian %>%
  mutate(cond = factor(cond, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN"))) %>%
  filter(clue %in% c(1,2,3,4))


save(d_cartesian, file = here("experiment-3/data/derived/data_cartesian.Rdata"))

#' Title
#'
#' @param data Full clean data frame
#' @param targetBlock Which block to filter by 
#' @param clue Which clue to filter by (1-4)
#' @param cond "UN" "US" "MS" "HN" "HS" "RS" "MN" "RN"
#'
#' @return Filtered data frame ready to be fit to the model
getBlockResponses = function(data, targetBlock, clue, cond){
  resp <-  data[data[,"cond"] == cond & data[,"block"] == targetBlock & data[,"clue"] == clue,]
  resp
  }

# Generate data sets
all_conditions <- expand.grid(
  clues = c(0:4),
  blocks = 1:8,
  conditions = unique(d_cartesian$cond)
)

all_conditions <- all_conditions %>%
  mutate(conditions = factor(conditions, levels = c("MS", "US", "RS", "HS", "MN", "UN", "RN", "HN")))

save(all_conditions, file = here("experiment-3/data/derived/all_conditions.Rdata"))

# Generate a data set for each condition
for (i in 1:length(all_conditions[,1])){
  tb <- all_conditions[i,"targetBlocks"]
  cond <- all_conditions[i,"conditions"]
  clue <- all_conditions[i,"clues"]
  d_cond <- getBlockResponses(d_cartesian, tb, clue, cond)
  save(d_cond, file = here(paste0("experiment-3/data/derived/",cond,"-clue-",clue,"-tb-",tb,".Rdata")))
}

d_subj <- d_cartesian %>%
  group_by(pid) %>%
  summarise(cond = unique(cond))

n = d_subj %>%
  group_by(cond) %>%
  summarise(n())


