library(here)
source(here("genericFunctions.R"))
source(here("calculatingFunctions.R"))

# generates the hypotheses and points for xrange by yrange space

xrange <- 0:3   # x axis range
yrange <- 0:3   # y axis range
alphas <- c(-5,-2,-1,-0.5,-0.1,0,0.1,0.5,1,2,5)  # range of alphas in calculation

# generate all of the hypotheses in this space
hyp <- makeBorders(xrange,yrange)
nHyp <- nrow(hyp)
# generate all points in this space
pts <- findAllPoints(xrange,yrange)
nPts <- nrow(pts)
pts$selected <- rep(FALSE,nPts)
pts$type <- rep(NA,nPts)
# initialise points to have uniform prior (can change later)
# initialise so posterior is the same as prior
pts$prior <- rep(1,nPts)/sum(rep(1,nPts))
pts$posterior <- pts$prior

# set up arrays to hold probabilities of points for each alpha
posProbPts <- array(data=0,dim=c(nPts,nHyp,length(alphas)),
                    dimnames=list (rownames(pts),rownames(hyp),
                                   rep("A",length(alphas))))
negProbPts <- array(data=0,dim=c(nPts,nHyp,length(alphas)),
                    dimnames=list (rownames(pts),rownames(hyp),
                                   rep("A",length(alphas))))
allProbPts <- array(data=0,dim=c(nPts,nHyp,length(alphas)),
                    dimnames=list (rownames(pts),rownames(hyp),
                                   rep("A",length(alphas))))

# calculate the probability of points for positive and negative for each alpha
for (a in 1:length(alphas)) {
  ppp <- findProbabilityOfPoints(hyp,pts,whichObs="pos",alpha=alphas[a])
  nnn <- findProbabilityOfPoints(hyp,pts,whichObs="neg",alpha=alphas[a])
  aaa <- ppp + nnn
  posProbPts[,,a] <- data.matrix(ppp/sum(ppp))
  negProbPts[,,a] <- data.matrix(nnn/sum(nnn))
  allProbPts[,,a] <- data.matrix(aaa/sum(aaa))
}

# create consistency matrix
consPts <- findConsistencyOfPoints(hyp,pts)

# save all of this as RData file
filename = paste0("datafiles/x",min(xrange),"to",max(xrange),"y",
                  min(yrange),"to",max(yrange),".RData")
save(xrange,yrange,alphas,hyp,nHyp,pts,nPts,posProbPts,negProbPts,
     allProbPts,consPts,file=filename)

