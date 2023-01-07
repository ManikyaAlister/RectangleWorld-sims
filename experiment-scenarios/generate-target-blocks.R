library(jsonlite)
rm(list = ls())
source(here("simulateMultipleAlphas.R"))

trueRect1 <- c(6,8,9,5)
trueRect2 <- 10-trueRect1
# Make target observations
observationsT1 <- data.frame(x = c(5.5, 8.5,5.5,9.5), y = c(2.5,5.5,5.5,3.5), category = c("positive","positive","positive","negative"))

observationsT2 <- observationsT1 %>%
  mutate(x = 10-x,
         y = 10-y)


# Convert to experiment format (not json, just grid structure instead of coordinate structure)
t1json <- observationsT1 %>%
  mutate(x = x+0.5,
         y = (10-y)+0.5)

t2json <- observationsT2 %>%
  mutate(x = x+0.5,
         y = (10-y)+0.5)

toJSON(t1json, pretty = TRUE, auto_unbox = TRUE)

# Size of rectangle world grid. 
H <- 10

# All learner alpha predictions to test 
allAlphas <- c(1,0.5,0.1,0,-0.1, -0.5, -1)

# visualise target blocks
target1 <- multiAlphaPredictions(observationsT1, save = TRUE)
target1
target2 <- multiAlphaPredictions(observationsT2, save = TRUE)
target2
