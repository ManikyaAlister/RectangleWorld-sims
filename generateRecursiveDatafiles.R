library(here)
source(here("functions/generic-functions.R"))
source(here("functions/calculatingFunctions.R"))

# size of hypotheses
H <- 10
xrange <- 0:H 
yrange <- 0:H

# names for saving all of this as three RData files
filenameMain = paste0("datafiles/x",min(xrange),"to",max(xrange),"y",
                      min(yrange),"to",max(yrange),"recursiveMain.RData")
filenameLow = paste0("datafiles/x",min(xrange),"to",max(xrange),"y",
                     min(yrange),"to",max(yrange),"recursiveLow.RData")
filenameHigh = paste0("datafiles/x",min(xrange),"to",max(xrange),"y",
                      min(yrange),"to",max(yrange),"recursiveHigh.RData")

# loads up the other data we need
fileSeg <- paste0("x0to",H,"y0to",H)
fn <- paste0("datafiles/",fileSeg,".RData")
load(here(fn))

# range of alphas in calculation
alphas <- c(-5,-2,-1,-0.5,-0.1,0,0.1,0.5,1,2,5)  

# useful constants
nH <- ncol(allProbPts)
nAlphas <- length(alphas)

# for each matrix, the third dimension varies alphas (reflecting lnTchAlpha, i.e.,
# what the learner assumes the teacher assumes the learner knows)
#  so lnTchAlpha=-1 means the learner assumes the teacher assumes the learner
#    knows the teacher is being deceptive

# ******************* MAIN THREE ****************

# D1: LEARNER ASSUMES TEACHER IS DECEPTIVE: lnAlpha = -1
D1allProbPts <- allProbPts
D1allProbPts[,,] <- NA
D1posProbPts <- D1allProbPts
D1negProbPts <- D1allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    D1allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=-1)
  }
  # create positive and negative versions by removing/changing negatives
  D1posProbPts[,,a] <- D1allProbPts[,,a]
  D1posProbPts[,,a][D1posProbPts[,,a]<0] <- 0
  D1negProbPts[,,a] <- D1allProbPts[,,a]
  D1negProbPts[,,a][D1negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  D1allProbPts[,,a] <- abs(D1allProbPts[,,a])
  D1negProbPts[,,a] <- abs(D1negProbPts[,,a])
  # normalise so all sums to one
  D1allProbPts[,,a] <- D1allProbPts[,,a]/sum(D1allProbPts[,,a])
  D1posProbPts[,,a] <- D1posProbPts[,,a]/sum(D1posProbPts[,,a])
  D1negProbPts[,,a] <- D1negProbPts[,,a]/sum(D1negProbPts[,,a])
}

# W: LEARNER ASSUMES TEACHER IS WEAK: lnAlpha = 0
WallProbPts <- allProbPts
WallProbPts[,,] <- NA
WposProbPts <- WallProbPts
WnegProbPts <- WallProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    WallProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=0)
  }
  # create positive and negative versions by removing/changing negatives
  WposProbPts[,,a] <- WallProbPts[,,a]
  WposProbPts[,,a][WposProbPts[,,a]<0] <- 0
  WnegProbPts[,,a] <- WallProbPts[,,a]
  WnegProbPts[,,a][WnegProbPts[,,a]>0] <- 0
  # now get rid of negatives
  WallProbPts[,,a] <- abs(WallProbPts[,,a])
  WnegProbPts[,,a] <- abs(WnegProbPts[,,a])
  # normalise so all sums to one
  WallProbPts[,,a] <- WallProbPts[,,a]/sum(WallProbPts[,,a])
  WposProbPts[,,a] <- WposProbPts[,,a]/sum(WposProbPts[,,a])
  WnegProbPts[,,a] <- WnegProbPts[,,a]/sum(WnegProbPts[,,a])
}

# H1: LEARNER ASSUMES TEACHER IS HELPFUL: lnAlpha = 1
H1allProbPts <- allProbPts
H1allProbPts[,,] <- NA
H1posProbPts <- H1allProbPts
H1negProbPts <- H1allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    H1allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=1)
  }
  # create positive and negative versions by removing/changing negatives
  H1posProbPts[,,a] <- H1allProbPts[,,a]
  H1posProbPts[,,a][H1posProbPts[,,a]<0] <- 0
  H1negProbPts[,,a] <- H1allProbPts[,,a]
  H1negProbPts[,,a][H1negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  H1allProbPts[,,a] <- abs(H1allProbPts[,,a])
  H1negProbPts[,,a] <- abs(H1negProbPts[,,a])
  # normalise so all sums to one
  H1allProbPts[,,a] <- H1allProbPts[,,a]/sum(H1allProbPts[,,a])
  H1posProbPts[,,a] <- H1posProbPts[,,a]/sum(H1posProbPts[,,a])
  H1negProbPts[,,a] <- H1negProbPts[,,a]/sum(H1negProbPts[,,a])
}


# split into three files when saving to aid memory
save(D1allProbPts,D1negProbPts,D1posProbPts,
     H1allProbPts,H1negProbPts,H1posProbPts,
     WallProbPts,WnegProbPts,WposProbPts,
     file=filenameMain)

# ******************* EXTREME FOUR **************** <--- Run to here!!! 


# D5: LEARNER ASSUMES TEACHER IS DECEPTIVE: lnAlpha = -5
D5allProbPts <- allProbPts
D5allProbPts[,,] <- NA
D5posProbPts <- D5allProbPts
D5negProbPts <- D5allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    D5allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=-5)
  }
  # create positive and negative versions by removing/changing negatives
  D5posProbPts[,,a] <- D5allProbPts[,,a]
  D5posProbPts[,,a][D5posProbPts[,,a]<0] <- 0
  D5negProbPts[,,a] <- D5allProbPts[,,a]
  D5negProbPts[,,a][D5negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  D5allProbPts[,,a] <- abs(D5allProbPts[,,a])
  D5negProbPts[,,a] <- abs(D5negProbPts[,,a])
  # normalise so all sums to one
  D5allProbPts[,,a] <- D5allProbPts[,,a]/sum(D5allProbPts[,,a])
  D5posProbPts[,,a] <- D5posProbPts[,,a]/sum(D5posProbPts[,,a])
  D5negProbPts[,,a] <- D5negProbPts[,,a]/sum(D5negProbPts[,,a])
}

# D2: LEARNER ASSUMES TEACHER IS DECEPTIVE: lnAlpha = -2
D2allProbPts <- allProbPts
D2allProbPts[,,] <- NA
D2posProbPts <- D2allProbPts
D2negProbPts <- D2allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    D2allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=-2)
  }
  # create positive and negative versions by removing/changing negatives
  D2posProbPts[,,a] <- D2allProbPts[,,a]
  D2posProbPts[,,a][D2posProbPts[,,a]<0] <- 0
  D2negProbPts[,,a] <- D2allProbPts[,,a]
  D2negProbPts[,,a][D2negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  D2allProbPts[,,a] <- abs(D2allProbPts[,,a])
  D2negProbPts[,,a] <- abs(D2negProbPts[,,a])
  # normalise so all sums to one
  D2allProbPts[,,a] <- D2allProbPts[,,a]/sum(D2allProbPts[,,a])
  D2posProbPts[,,a] <- D2posProbPts[,,a]/sum(D2posProbPts[,,a])
  D2negProbPts[,,a] <- D2negProbPts[,,a]/sum(D2negProbPts[,,a])
}


# H2: LEARNER ASSUMES TEACHER IS HELPFUL: lnAlpha = 2
H2allProbPts <- allProbPts
H2allProbPts[,,] <- NA
H2posProbPts <- H2allProbPts
H2negProbPts <- H2allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    H2allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=2)
  }
  # create positive and negative versions by removing/changing negatives
  H2posProbPts[,,a] <- H2allProbPts[,,a]
  H2posProbPts[,,a][H2posProbPts[,,a]<0] <- 0
  H2negProbPts[,,a] <- H2allProbPts[,,a]
  H2negProbPts[,,a][H2negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  H2allProbPts[,,a] <- abs(H2allProbPts[,,a])
  H2negProbPts[,,a] <- abs(H2negProbPts[,,a])
  # normalise so all sums to one
  H2allProbPts[,,a] <- H2allProbPts[,,a]/sum(H2allProbPts[,,a])
  H2posProbPts[,,a] <- H2posProbPts[,,a]/sum(H2posProbPts[,,a])
  H2negProbPts[,,a] <- H2negProbPts[,,a]/sum(H2negProbPts[,,a])
}


# H5: LEARNER ASSUMES TEACHER IS HELPFUL: lnAlpha = 5
H5allProbPts <- allProbPts
H5allProbPts[,,] <- NA
H5posProbPts <- H5allProbPts
H5negProbPts <- H5allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    H5allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=5)
  }
  # create positive and negative versions by removing/changing negatives
  H5posProbPts[,,a] <- H5allProbPts[,,a]
  H5posProbPts[,,a][H5posProbPts[,,a]<0] <- 0
  H5negProbPts[,,a] <- H5allProbPts[,,a]
  H5negProbPts[,,a][H5negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  H5allProbPts[,,a] <- abs(H5allProbPts[,,a])
  H5negProbPts[,,a] <- abs(H5negProbPts[,,a])
  # normalise so all sums to one
  H5allProbPts[,,a] <- H5allProbPts[,,a]/sum(H5allProbPts[,,a])
  H5posProbPts[,,a] <- H5posProbPts[,,a]/sum(H5posProbPts[,,a])
  H5negProbPts[,,a] <- H5negProbPts[,,a]/sum(H5negProbPts[,,a])
}


save(D5allProbPts,D5negProbPts,D5posProbPts,
     D2allProbPts,D2negProbPts,D2posProbPts,
     H2allProbPts,H2negProbPts,H2posProbPts,
     H5allProbPts,H5negProbPts,H5posProbPts,
     file=filenameHigh)


# ******************* PATHETIC FOUR ****************


# D05: LEARNER ASSUMES TEACHER IS DECEPTIVE: lnAlpha = -0.5
D05allProbPts <- allProbPts
D05allProbPts[,,] <- NA
D05posProbPts <- D05allProbPts
D05negProbPts <- D05allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    D05allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=-0.5)
  }
  # create positive and negative versions by removing/changing negatives
  D05posProbPts[,,a] <- D05allProbPts[,,a]
  D05posProbPts[,,a][D05posProbPts[,,a]<0] <- 0
  D05negProbPts[,,a] <- D05allProbPts[,,a]
  D05negProbPts[,,a][D05negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  D05allProbPts[,,a] <- abs(D05allProbPts[,,a])
  D05negProbPts[,,a] <- abs(D05negProbPts[,,a])
  # normalise so all sums to one
  D05allProbPts[,,a] <- D05allProbPts[,,a]/sum(D05allProbPts[,,a])
  D05posProbPts[,,a] <- D05posProbPts[,,a]/sum(D05posProbPts[,,a])
  D05negProbPts[,,a] <- D05negProbPts[,,a]/sum(D05negProbPts[,,a])
}


# D01: LEARNER ASSUMES TEACHER IS DECEPTIVE: lnAlpha = -0.1
D01allProbPts <- allProbPts
D01allProbPts[,,] <- NA
D01posProbPts <- D01allProbPts
D01negProbPts <- D01allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    D01allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=-0.1)
  }
  # create positive and negative versions by removing/changing negatives
  D01posProbPts[,,a] <- D01allProbPts[,,a]
  D01posProbPts[,,a][D01posProbPts[,,a]<0] <- 0
  D01negProbPts[,,a] <- D01allProbPts[,,a]
  D01negProbPts[,,a][D01negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  D01allProbPts[,,a] <- abs(D01allProbPts[,,a])
  D01negProbPts[,,a] <- abs(D01negProbPts[,,a])
  # normalise so all sums to one
  D01allProbPts[,,a] <- D01allProbPts[,,a]/sum(D01allProbPts[,,a])
  D01posProbPts[,,a] <- D01posProbPts[,,a]/sum(D01posProbPts[,,a])
  D01negProbPts[,,a] <- D01negProbPts[,,a]/sum(D01negProbPts[,,a])
}



# H01: LEARNER ASSUMES TEACHER IS HELPFUL: lnAlpha = 0.1
H01allProbPts <- allProbPts
H01allProbPts[,,] <- NA
H01posProbPts <- H01allProbPts
H01negProbPts <- H01allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    H01allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=0.1)
  }
  # create positive and negative versions by removing/changing negatives
  H01posProbPts[,,a] <- H01allProbPts[,,a]
  H01posProbPts[,,a][H01posProbPts[,,a]<0] <- 0
  H01negProbPts[,,a] <- H01allProbPts[,,a]
  H01negProbPts[,,a][H01negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  H01allProbPts[,,a] <- abs(H01allProbPts[,,a])
  H01negProbPts[,,a] <- abs(H01negProbPts[,,a])
  # normalise so all sums to one
  H01allProbPts[,,a] <- H01allProbPts[,,a]/sum(H01allProbPts[,,a])
  H01posProbPts[,,a] <- H01posProbPts[,,a]/sum(H01posProbPts[,,a])
  H01negProbPts[,,a] <- H01negProbPts[,,a]/sum(H01negProbPts[,,a])
}


# H05: LEARNER ASSUMES TEACHER IS HELPFUL: lnAlpha = 0.5
H05allProbPts <- allProbPts
H05allProbPts[,,] <- NA
H05posProbPts <- H05allProbPts
H05negProbPts <- H05allProbPts
# do this for each of the lnTchAlpha assumptions
for (a in 1:nAlphas) {
  # get sampling distribution for each hypothesis at that alpha
  for (i in 1:nH) {
    H05allProbPts[,i,a] <- 
      getSamplingDistribution(allProbPts[,,a],consPts,pts,i,
                              priors=hyp$prior,alpha=0.5)
  }
  # create positive and negative versions by removing/changing negatives
  H05posProbPts[,,a] <- H05allProbPts[,,a]
  H05posProbPts[,,a][H05posProbPts[,,a]<0] <- 0
  H05negProbPts[,,a] <- H05allProbPts[,,a]
  H05negProbPts[,,a][H05negProbPts[,,a]>0] <- 0
  # now get rid of negatives
  H05allProbPts[,,a] <- abs(H05allProbPts[,,a])
  H05negProbPts[,,a] <- abs(H05negProbPts[,,a])
  # normalise so all sums to one
  H05allProbPts[,,a] <- H05allProbPts[,,a]/sum(H05allProbPts[,,a])
  H05posProbPts[,,a] <- H05posProbPts[,,a]/sum(H05posProbPts[,,a])
  H05negProbPts[,,a] <- H05negProbPts[,,a]/sum(H05negProbPts[,,a])
}



save(D05allProbPts,D05negProbPts,D05posProbPts,
     D01allProbPts,D01negProbPts,D01posProbPts,
     H01allProbPts,H01negProbPts,H01posProbPts,
     H05allProbPts,H05negProbPts,H05posProbPts,
     file=filenameLow)





