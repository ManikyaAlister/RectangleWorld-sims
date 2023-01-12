rm(list = ls())
# Simulate target trials --------------------------------------------------
# source simulation function
source(here("simulateExperimentBlock.R"))
# load data 
load(here("experiment-scenarios/target-blocks/data/target-trial-1-Cartesian.Rdata"))
load(here("experiment-scenarios/target-blocks/data/target-trial-2-Cartesian.Rdata"))


# Target trial 1 ----------------------------------------------------------

# Ground truth rectangle
trueHT1 <- targetTrial1$groundTruth

# Observations
obsT1 <- targetTrial1$observations

# Plot heat map of distribution
t1_trusting <- simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = 1, prior = "normal")
t1_trusting
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_trusting.png"), width = 10)


t1_random <- simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = 0, prior = "normal")
t1_random
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_random.png"), width = 10)


t1_suspicious <-simulateExperimentBlock(obsT1, trueHT1, learnerAlpha = -1, prior = "normal")
t1_suspicious
ggsave(filename = here("experiment-scenarios/simulate-distributions/learner-distribution-figures/t1_suspicious.png"), width = 10)

# Plot posterior probability of rectangles given these points in each condition
source(here("getLearnerHypDistributions.R"))

observations = obsT1

trustingLearner = getLearnerHypDistribution(observations, alpha = 1, nTrials = 4)
susLearner = getLearnerHypDistribution(observations, alpha = -1, nTrials = 4)
randomLearner = getLearnerHypDistribution(observations, alpha = 0, nTrials = 4)

# 1 observation

trustingLearner1 = trustingLearner[[1]]
susLearner1 = susLearner[[1]]
randomLearner1 = randomLearner[[1]]

# Order rectangle size
size1orderedIndex <- order(trustingLearner1[,"size"], decreasing = TRUE)
size1ordered <- trustingLearner1[size1orderedIndex, "size"]

# combine different learners to 1 data frame
combined1 <- cbind(susLearner1[,"posterior"], trustingLearner1[,"posterior"], randomLearner1[,"posterior"])

# Order by rectangle size
ordered1 <- combined1[size1orderedIndex,]

plot(size1ordered,ordered1[,1], type = "l", ylim = c(0,0.005), col = "red", ylab = "Posterior", xlab = "Rectangle (ordered by size)")
lines(size1ordered,ordered1[,2], col = "green")
lines(size1ordered,ordered1[,3], col = "blue")

# 2 observations
trustingLearner2 = trustingLearner[[2]]
susLearner2 = susLearner[[2]]
randomLearner2 = randomLearner[[2]]

# Order rectangle size
size2orderedIndex <- order(trustingLearner2[,"size"], decreasing = TRUE)
size2ordered <- trustingLearner2[size2orderedIndex, "size"]


# combine different learners to 1 data frame
combined2<- cbind(susLearner2[,"posterior"], trustingLearner2[,"posterior"], randomLearner2[,"posterior"])
# Order by the trusting learner's posterior
ordered2 <- combined2[size2orderedIndex,]


plot(size2ordered,ordered2[,1], type = "l", ylim = c(0,0.05), col = "red", ylab = "Posterior", xlab = "Rectangle (ordered by size)")
lines(size2ordered,ordered2[,2], col = "green")
lines(size2ordered,ordered2[,3], col = "blue")

# 3 observations
trustingLearner3 = trustingLearner[[3]]
susLearner3 = susLearner[[3]]
randomLearner3 = randomLearner[[3]]

# Order rectangle size
size3orderedIndex <- order(trustingLearner3[,"size"], decreasing = TRUE)
size3ordered <- trustingLearner3[size2orderedIndex, "size"]


# combine different learners to 1 data frame
combined3<- cbind(susLearner3[,"posterior"], trustingLearner3[,"posterior"], randomLearner3[,"posterior"])
# Order by the trusting learner's posterior
ordered3 <- combined3[size3orderedIndex,]


plot(size3ordered,ordered3[,1], type = "l", ylim = c(0,0.05), col = "red", ylab = "Posterior", xlab = "Rectangle (ordered by size)")
lines(size3ordered,ordered3[,2], col = "green")
lines(size3ordered,ordered3[,3], col = "blue")





observations = obsT1[1:2,]

trustingLearner = getLearnerHypDistribution(observations, alpha = 1, nTrials = 2)
trustingLearner = trustingLearner[[2]]

susLearner = getLearnerHypDistribution(observations, alpha = -1, nTrials = 2)
susLearner = susLearner[[2]]

randomLearner = getLearnerHypDistribution(observations, alpha = 0, nTrials = 2)
randomLearner = randomLearner[[2]]

combined <- cbind(susLearner[,"posterior"], trustingLearner[,"posterior"], randomLearner[,"posterior"])
ordered <- combined[order(trustingLearner[,"posterior"], decreasing = TRUE),]

plot(ordered[1:200,1], type = "l", ylim = c(0,0.01), col = "red")
lines(ordered[1:200,2], col = "green")
lines(ordered[1:200,3], col = "blue")

# 3 observations

observations = obsT1[1:3,]

trustingLearner = getLearnerHypDistribution(observations, alpha = 1, nTrials = 3)
trustingLearner = trustingLearner[[3]]

susLearner = getLearnerHypDistribution(observations, alpha = -1, nTrials = 3)
susLearner = susLearner[[3]]

randomLearner = getLearnerHypDistribution(observations, alpha = 0, nTrials = 3)
randomLearner = randomLearner[[3]]

combined <- cbind(susLearner[,"posterior"], trustingLearner[,"posterior"], randomLearner[,"posterior"])
ordered <- combined[order(trustingLearner[,"posterior"], decreasing = TRUE),]

plot(ordered[1:200,1], type = "l", ylim = c(0,0.02), col = "red")
lines(ordered[1:200,2], col = "green")
lines(ordered[1:200,3], col = "blue")

# 4 observations

observations = obsT1[1:4,]

trustingLearner = getLearnerHypDistribution(observations, alpha = 1, nTrials = 4)
trustingLearner = trustingLearner[[4]]

susLearner = getLearnerHypDistribution(observations, alpha = -1, nTrials = 4)
susLearner = susLearner[[4]]

randomLearner = getLearnerHypDistribution(observations, alpha = 0, nTrials = 4)
randomLearner = randomLearner[[4]]

combined <- cbind(susLearner[,"posterior"], trustingLearner[,"posterior"], randomLearner[,"posterior"])
ordered <- combined[order(trustingLearner[,"posterior"], decreasing = TRUE),]

plot(ordered[1:100,1], type = "l", ylim = c(0,0.03), col = "red")
lines(ordered[1:100,2], col = "green")
lines(ordered[1:100,3], col = "blue")
