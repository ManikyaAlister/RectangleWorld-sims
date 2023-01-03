rm(list = ls())
source(here("experiment-scenarios/multiAlphaPredictions.R"))

# Make target observations
observationsT2 <- data.frame(x = c(10-5.5, 10-8.5,10-6.5,10-9.5), y = c(10-2.5,10-5.5,10-1.5,10-6.5), category = c("positive","positive","negative","negative"))
observationsT1 <- data.frame(x = c(5.5, 8.5,6.5,9.5), y = c(2.5,5.5,1.5,6.5), category = c("positive","positive","negative","negative"))


# Size of rectangle world grid. 
H <- 10

# All learner alpha predictions to test 
allAlphas <- c(1,0.5,0.1,0,-0.1, -0.5, -1)

# generate target blocks
target1 <- multiAlphaPredictions(observationsT1, save = TRUE)
target1
target2 <- multiAlphaPredictions(observationsT2, save = TRUE)
target2