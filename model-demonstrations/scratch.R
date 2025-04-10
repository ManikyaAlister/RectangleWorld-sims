library(here)
source(here("functions/genericFunctions.R"))

xrange <- 0:5   # x axis range
yrange <- 0:5   # y axis range
alphas <- c(-2,-1,-0.5,-0.1,0,0.1,0.5,1,2)

# generate all of the hypotheses in this space
hyp <- makeBorders(xrange,yrange)
nHyp <- nrow(hyp)
# generate all points in this space
pts <- findAllPoints(xrange,yrange)
nPts <- nrow(pts)

posProbPts <- array(data=0,dim=c(nPts,nHyp,length(alphas)),
                    dimnames=list (rownames(pts),rownames(hyp),
                                   rep("A",length(alphas))))
negProbPts <- array(data=0,dim=c(nPts,nHyp,length(alphas)),
                    dimnames=list (rownames(pts),rownames(hyp),
                                   rep("A",length(alphas))))

for (a in 1:length(alphas)) {
  ppp <- findProbabilityOfPoints(hyp,pts,whichObs="pos",alpha=alphas[a])
  posProbPts[,,a] <- data.matrix(ppp/sum(ppp))
  nnn <- findProbabilityOfPoints(hyp,pts,whichObs="neg",alpha=alphas[a])
  negProbPts[,,a] <- data.matrix(nnn/sum(nnn))
}

filename = paste0("datafiles/x",min(xrange),"to",max(xrange),"y",
                  min(yrange),"to",max(yrange),".RData")
save(xrange,yrange,alphas,hyp,nHyp,pts,nPts,posProbPts,negProbPts,
     file=filename)



# and calculate the probability of each point by summing over all hypotheses
allPosPtProbs <- sweep(posProbPts,2,hyp$posterior,"*")
allNegPtProbs <- sweep(negProbPts,2,hyp$posterior,"*")
allPtProbs <- allPosPtProbs + allNegPtProbs
pts$negProb <- rowSums(allNegPtProbs)/sum(allNegPtProbs)
pts$posProb <- rowSums(allPosPtProbs)/sum(allPosPtProbs)
pts$prob <- rowSums(allPtProbs)/sum(allPtProbs)

# put them all in a list
myList <- list(xrange,yrange,alpha,hyp,pts,posProbPts,negProbPts)

st <- paste0("Alpha=",alpha,"; positive observations")
plotDistribution(allPts=pts,xrange=xrange,yrange=yrange,whichObs="pos",subtitle=st)
st <- paste0("Alpha=",alpha,"; negative observations")
plotDistribution(allPts=pts,xrange=xrange,yrange=yrange,whichObs="neg",subtitle=st)


nNeg <- 1    # number of negative points we want
nPos <- 2     # number of positive points we want
trueRectangle <- c(0,1,2,2)     # the true rectangle
nBest = 3         # number of "best" hypotheses we want if there is no single best

# total number of observations
nObs <- nNeg + nPos

# let's now sample some points weakly
wsObs <- weakSampler(nObs,trueRectangle,xrange,yrange)
hyp$nPos <- sum(wsObs$category=="positive")
hyp$nNeg <- sum(wsObs$category=="negative")
wsPlot <- plotHypotheses(trueRectangle,wsObs,rects=NA,xrange,yrange)
wsPlot

# let's sample some points strongly
srObs <- sampleRect(nPos,nNeg,trueRectangle,xrange,yrange)
hyp$nPos <- nPos
hyp$nNeg <- nNeg
srPlot <- plotHypotheses(trueRectangle,srObs,rects=NA,xrange,yrange)
srPlot

## recalculate hypothesis probability based on weak sampling
hyp$consPos <- ruleOutHypotheses(pts,consPt,obs=srObs,whichObs="pos")
hyp$consNeg <- ruleOutHypotheses(pts,consPt,obs=srObs,whichObs="neg")
hyp$likePos <- as.numeric(hyp$consPos)
hyp$likeNeg <- as.numeric(hyp$consNeg)
hyp$posterior <- hyp$prior*hyp$likePos*hyp$likeNeg
hyp$posterior <- hyp$posterior/sum(hyp$posterior)
# recalculate probability of each point now for plotting
allPtProbs <- sweep(consPt,2,hyp$posterior,"*")
pts$prob <- rowSums(allPtProbs)/sum(allPtProbs)

# let's make a plot of the distribution
distPlot <- plotDistribution(srObs,trueRectangle,pts,xrange,yrange)
distPlot

# let's make a plot of the best hypotheses
bestH <- returnBestHypothesis(hyp,n=nBest)
pBestH <- plotHypotheses(trueRectangle,srObs,hyp[bestH,],xrange,yrange)

