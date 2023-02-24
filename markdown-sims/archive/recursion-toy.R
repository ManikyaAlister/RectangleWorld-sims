library(here)

# sourcing our files
source(here("genericFunctions.R"))
source(here("calculatingFunctions.R"))
source(here("plottingFunctions.R"))

## Toy example
# Five hypotheses, each a different size 
size = 1:5
# Level 0 is a learner who is naive (weak sampling) 
# what would a teacher do if they're trying to help/deceive naive learner
lik_dec_0 = (1/size)^(0)

# need to integrate the point matrix


# Level 1 learner assumes a level 0 teacher. 

# Level 1 teacher assumes a level 1 learner. 
# first prior(uniform)
prior = 1/length(size)

# teacher likelihood
l_teacher_dec <- (1/size)^(-1)

# posterior of teacher
p_teacher_dec <- (prior * l_teacher_dec)/sum((prior * l_teacher_dec))^-1


# Posterior of learner 
p_learner_dec <- (prior*p_teacher_dec)/sum((prior*p_teacher_dec))




# Likelihoods of first layers of recursion
lik_dec = (1/size)^(-1)
lik_help = (1/size)^(1)

# first prior(uniform)
prior = 1/length(size)

# posteriors of first layer of recursion
prob_dec = (prior*lik_dec)/sum(prior*lik_dec)
prob_help = (prior*lik_help)/sum(prior*lik_help)

# plot
plot(size,prob_dec, "l",ylim = c(0,1), col = "green")
lines(size,prob_help, col="blue")

# second layer of recursion (deceptive assumption only)

# prior = posterior of first layer of recursion 
prior_rec_dec = prob_dec

# likelihood = likelihood of first layer raised to the power of alpha again
lik_rec_dec = lik_dec^-1 

# posterior = prior * likelihood/sum()
prob_rec_dec = (prior_dec*lik_rec_dec)/sum(prior_rec_dec*lik_rec_dec)

# plot all: likelihoods
plot(size,lik_dec, "l",ylim = c(0,6), col = "green")
lines(size,lik_help, col="blue")
lines(size,lik_rec_dec, col = "red")

# plot all: posteriors
plot(size,prob_dec, "l",ylim = c(0,1), col = "green")
lines(size,prob_help, col="blue")
lines(size,prob_rec_dec, col = "red")


# 
# H <- 10
# 
# # load the pre-calculate data
# fileSeg <- paste0("x0to",H,"y0to",H)
# fn <- paste0("datafiles/",fileSeg,".RData")
# load(here(fn))
# 
# # set up default learner hypotheses
# lnHyp <- hyp
# 
# # set up way to get indexes of points
# pts$index = 1:length(pts[,1])
# getPtIndex = function(pt){
#   index = pts[pts[,"x"] == pt[,"x"] & pts[,"y"] == pt[,"y"], "index"]
#   index
# }
# 
# # set alphas based on parameters above
# lnAlpha = -1
# lA <- which(alphas==lnAlpha)
# lnAlphaText <- returnAlpha(lnAlpha)
# 
# # 1. Learner observers some points 
# load(here("experiment-scenarios/target-blocks/data/target-block-8-Cartesian.Rdata"))
# observations <- targetBlock$observations
# newPt <- observations[1,]
# 
# # get point index
# newPt$index = getPtIndex(newPt)
# 
# # 2. Learner calculates the probability of the teacher choosing each point given a certain alpha level, then uses that 
# # to determine the probability of each hypothesis given that alpha.
# lnHyp <- updateHypotheses(allProbPts[,,lA],consPts,newPt,lnHyp)
# lnHyp$recursion <- 1
# # Plot distribution of rectangles by size 
# 
# # plot
# lnHyp %>%
#   arrange(size) %>%
#   filter(posterior > 0)%>%
#   ggplot()+
#   geom_line(aes(x = size, y = posterior))+
#   theme_classic()
# 
# # Extra recursion layer 
# 
# # Update point distribution of the learner once more: 
# # What is the probability of each point for each hypothesis? Calculate likelihood with alpha one extra time for extra recursion. 
 ppp <- findProbabilityOfPoints(lnHyp,pts,whichObs="pos",alpha=lnAlpha)
 nnn <- findProbabilityOfPoints(lnHyp,pts,whichObs="neg",alpha=lnAlpha)
 aaa <- ppp + nnn
# lnPts <- data.matrix(aaa/sum(aaa))
# 
# # 3. With updated point distribution, get learner's new hypothesis distribution given extra layer of recursion. 
# lnHyp2 <- updateHypotheses(lnPts,consPts,newPt,lnHyp)
# lnHyp2$recursion <- 2
# 
# # combine
# lnHypRecursive <- rbind(lnHyp, lnHyp2)
# 
# # plot
# lnHypRecursive %>%
#   arrange(size) %>%
#   filter(posterior > 0)%>%
#   ggplot()+
#   geom_line(aes(x = size, y = posterior, colour = as.factor(recursion)))+
#   theme_classic()
# 
# 
# 
