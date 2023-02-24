library(here)

# sourcing our files
source(here("genericFunctions.R"))
source(here("calculatingFunctions.R"))
source(here("plottingFunctions.R"))

tchAlpha <- -1


trueH <- c(1,0,2,2)

# Step 1: Learner observes a point
newPt <- data.frame(x = 1.5, y = 1.5, category = "positive")
newPt$index = getPtIndex(newPt)

tchHyp <- hyp

# Learner infers the teacher's estimate of the learner's distribution over hypotheses, given the point that was generated
tchHyp <- updateHypotheses(allProbPts[,,tlA],consPts,newPt,tchHyp)
bestH <- returnBestHypothesis(tchHyp,n=nBestH)
t <- "Teacher hypothesis distribution for learner"
pt1step3 <- plotHypotheses(trueH,newPt,tchHyp[bestH,],xrange=xrange,
                           yrange=yrange,title=t)

plotHypDist(tchHyp)

# Learner infers the teacher's estimate of the learner's distribution over points, given the hypothesis distribution
tchPts <- updatePoints(posProbPts[,,tlA],newPt,
                       posterior=abs(tchHyp$posterior),pts=tchPts)
t <- "Teacher point distribution for learner"
pt1step4 <- plotColourfulDistribution(allPts=tchPts,xrange=xrange,yrange=yrange,
                                      trueRectangle=trueH,obs=newPt,
                                      title=t)



# Learner then needs to work backwards to infer what the true hypothesis is, given what the teacher is trying to do (and their assumptions about the learner)

# Learner takes the probability of each point given each hypothesis the teacher thinks they has 
tchPtsMatrix <- posProbPts[,,tlA]

# Then updates that matrix one more time, applying alpha again to infer what the actual probability over points is. 
lnPtsMatrix <- p_tch_d_h(-1, tchHyp, pts, tchPtsMatrix, "pos")
pNewPt <- lnPtsMatrix[newPt$index,]

# Learner infers the teacher's actual distribution over hypotheses given their alpha
lnTchHyp <- tchHyp %>%
  mutate(posterior = pNewPt/sum(pNewPt))

plotHypDist(lnTchHyp)
# Learner infers the teacher's point X hypothesis distribution given what the teacher knows about the learner alpha (tchLnAlpha) and the teachers alpha (tchAlpha)



# learner updates what they think the teachers distribution over points is given their intentions 
lnPts <- 

# teacher updates their own sampling dist


# # level 0:  learner who is naive (weak sampling)
# 
# # level 1: teacher tries to shift belief according to their goals (alpha)
# 
# # level 2: learner belief back 
# 
# 
# H <- 3
# 
# # load the pre-calculate data
# fileSeg <- paste0("x0to",H,"y0to",H)
# fn <- paste0("datafiles/",fileSeg,".RData")
# load(here(fn))
# 
# # set up default learner hypotheses
# hyp
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
# # Learner observers some points
# newPt <- data.frame(x = 1.5, y = 1.5, category = "positive")
# 
# 
# 
# # get point index
# newPt$index = getPtIndex(newPt)
# 
# # Function to get the point X hypothesis matrix pTeacher(d|h)
# p_tch_d_h = function(alpha, hyp, pts){
#   ppp <- findProbabilityOfPoints(hyp,pts,whichObs="pos",alpha=alpha)
#   nnn <- findProbabilityOfPoints(hyp,pts,whichObs="neg",alpha=alpha)
#   aaa <- ppp + nnn
#   matrix <- data.matrix(aaa/sum(aaa))
#   matrix
# }
# 
# plotHypDist = function(hyp){
#   hyp %>%
#     filter(posterior>0) %>%
#     arrange(size) %>%
#     ggplot()+
#     geom_line(aes(x = size, y = posterior))
# }
# 
# 
# ## Recursion Level 1: Naive learner who trusts teacher 
# 
# # distribution over points that the learner thinks the teacher has 
# pTchLn_1 <- p_tch_d_h(alpha = 1, hyp, pts) 
# 
# # hypothesis distribution of learner given naive assumption
# pLn_1 <- updateHypotheses(pTchLn_1, consPts, newPt, hyp)
# 
# # plot
# plotHypDist(pLn_1)
# 
# 
# ## Recursion Level 2: Teacher is actually trying to mislead
# # but what if teacher was actually trying to deceive? They would guess the learner's naive hypothesis distribution
# # and rather than maximise their belief in the true hypothesis, they would try to minimize it. 
# pTchLn_2 <- p_tch_d_h(alpha = -1, pLn_1, pts) 
# 
# # new learner distribution over hypotheses 
# pLn_2 <-  updateHypotheses(pTchLn_2, consPts, newPt, pLn_1)
# 
# # plot
# plotHypDist(pLn_2)
# 
# 
# 
# # pTeacher(d|h): Probability of choosing each data point given each hypothesis and the alpha the teacher thinks the learner has
# p_teacher_0 <- p_tch_d_h(alpha = 0, hyp, pts) 
# 
# # pLearner(h|d): Probability of the learner selecting each hypothesis given the distribution over points they think the teacher has, 
# # plus their own alpha for the teacher
# p_learner_0 <- updateHypotheses(p_teacher_0,consPts,newPt,hyp)
# 
# # Update the probability of the learner choosing each point
# pts0 <- pts
# pts0$posterior <- rowSums(p_teacher_0)
# 
# # plot
# p_learner_0 %>%
#   filter(posterior>0) %>%
#   arrange(size) %>%
#   ggplot()+
#   geom_line(aes(x = size, y = posterior))
# 
# 
# # step 1 teacher tries to deceive assuming the learner is naive, learner behaves naive
# 
# # pTeacher(d|h)
# p_teacher_dec <- p_tch_d_h(alpha = -1, p_learner_0, pts0)
# 
# # pLearner(h|d)
# p_learner_dec <- updateHypotheses(p_teacher_dec,consPts,newPt,p_learner_0)
# 
# # update points 
# pts_dec <- pts0
# pts_dec$posterior <- rowSums(p_teacher_dec)
# 
# # plot
# p_learner_dec %>%
#   filter(posterior>0) %>%
#   arrange(size) %>%
#   ggplot()+
#   geom_line(aes(x = size, y = posterior))
# 
# 
# # step 2 teacher tries to deceive assuming the learner is suspicious, learner behaves suspicious
# p_teacher_dec2 <- p_tch_d_h(alpha = -1, p_learner_dec, pts_dec)
# p_learner_dec2 <- updateHypotheses(p_teacher_dec2,consPts,newPt,p_learner_dec)
# 
# p_learner_dec2 %>%
#   filter(posterior>0) %>%
#   arrange(size) %>%
#   ggplot()+
#   geom_line(aes(x = size, y = posterior))
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
#  ppp <- findProbabilityOfPoints(lnHyp,pts,whichObs="pos",alpha=lnAlpha)
#  nnn <- findProbabilityOfPoints(lnHyp,pts,whichObs="neg",alpha=lnAlpha)
#  aaa <- ppp + nnn
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



