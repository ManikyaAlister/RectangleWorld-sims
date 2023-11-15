library(dplyr)
#library(nnet)
source(here("genericFunctions.R"))



# ********************************
#     findConsistencyOfPoints
# ********************************
#' Given a data frame with a set of points, returns a data frame where each row corresponds to a point
#' and each column corresponds to a hypothesis. The cell is TRUE if that point
#' is in that hypothesis and false if it is not
#' @param hyp Data frame of hypotheses (each row is one, columns are x1,y1,x2,y2,prob)
#' @param pts Data frame of points (each row is one, columns are x and y)
#' @return A dataframe containing information about all points
#' the rownames correspond to the points in pts
#' the colnames correspond to the hypotheses in hyp

findConsistencyOfPoints = function(hyp,pts){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(FALSE,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (isInRectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
        consPt[p,h] <- TRUE
      } 
    }
  }
  
  consPt <- data.frame(consPt) 
  return(consPt)
}



# ********************************
#     findProbabilityOfPoints
# ********************************
#' Given a data frame with a set of points and another dataframe with a set of hypotheses
#' with weights, returns a dataframe where each row corresponds to a point
#' and each column corresponds to a hypothesis. The cell gives the probability
#' of that point for that hypothesis, depending on alpha. 
#' @param hyp Data frame of hypotheses (each row is one, columns are x1,y1,x2,y2,prob)
#' @param pts Data frame of points (each row is one, columns are x and y)
#' @param whichObs Says whether the points are positive or negative ("pos" or "neg")
#' @param alpha If zero, this is weak. -1 is deceptive. 1 is helpful. default=0
#' @return A dataframe containing information about all points
#' the rownames correspond to the points in pts
#' the colnames correspond to the hypotheses in hyp

findProbabilityOfPoints = function(hyp,pts,whichObs,alpha=0){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(0,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (isInRectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
        if (whichObs=="pos") {
          consPt[p,h] <- (1/hyp$size[h])^alpha
        } 
      } else {
        if (whichObs=="neg") {
          consPt[p,h] <- (1/hyp$negSize[h])^alpha 
        }
      }
    }
  }
  
  consPt <- data.frame(consPt) 
  return(consPt)
}




# ******************************
#     getSamplingDistribution
# ******************************
#' @title Returns the probability distribution for sampling the next point
#' @param probPts pts x hyp matrix with probabilites of each pt at each hyp
#' @param consPts pts x hyp matrix with TRUE where pt is in hyp FALSE otherwise
#' @param pts Set of points, where each row is one
#' @param targetHyp True rectangle, indicated as index in hyps
#' @param priors Priors of hypotheses (default: uniform)
#' @param obs Previous observations; necessary so as to not repeat (default: NA)
#' @param alpha What kind of sampling should be done (default: 0)
#' @return a vector of length nPts containing the probability of each
getSamplingDistribution = function(probPts, consPts, pts, targetHyp, priors=NULL, 
                                   obs=NA, alpha=0) {
  
  # first some cleaning and tidying
  numNewObs <- 1
  nObs <- 0
  origPts <- pts
  nPts <- nrow(probPts)
  nHyp <- ncol(probPts)
  pts$index <- 1:nPts
  ptsLeft <- 1:nPts
  if (is.null(priors)) {
    priors <- rep(1,nHyp)
    priors <- priors/sum(priors)
  }
  
  # check to make sure it is possible to sample without replacement
  if (!is.null(nrow(obs))) {
    # finds the points remaining
    pts$posterior[obs$index] <- 0
    nObs <-length(obs$index)
    probPts[obs$index,] <- 0
  }
  
  if (nPts - nObs - numNewObs < 0) {
    stop("sampleNextPoints: not enough points left")      
  } 
  
  # figure out which points/hypotheses are consistent with target
  th <- consPts[,targetHyp]
  # calculate the posterior for each point/hypothesis combo
  probPts <- probPts*(consPts==th)
  postPts <- sweep(probPts,2,priors,"*")
  # calculate posterior of the target hypothesis if that point is chosen
  postTH <- postPts[,targetHyp]/rowSums(postPts)
  postTH <- (postTH/sum(postTH,na.rm=TRUE))^alpha ## This is where the teacher applies their goals 
  # remove the observations so it's not going into the calculation
  if (!is.null(nrow(obs))) {
    postTH[obs$index] <- 0
  }
  # next line makes it so deceptive can't actually lie
  if (alpha<0) {
    postTH[is.infinite(postTH)] <- 0
  }
  # renormalise
  postTH <- postTH/sum(postTH,na.rm=TRUE)
  
  # convert so it looks nice when plotted
  mval <- min(postTH,na.rm=TRUE)/10
  postTH <- postTH + runif(n=nPts,min=-1*mval,max=mval)
  postTH <- postTH/sum(postTH,na.rm=TRUE)
  p <- 1*consPts[,targetHyp]
  p[p==0] <- -1 ## Because of this, need to make sure anything using these distributions is the absolute value. 
  postTH <- postTH*p
  return(postTH)
  
}



# *****************************
#     returnBestHypothesis
# *****************************
#' Given a set of hypotheses with probabilities, returns the number of the best hypothesis
#' If there are multiple then it returns n random ones with that probability
#' chosen to reflect the range of sizes there are. If n is greater than the number of 
#' best hypotheses then it returns all of the best hypotheses
#' @param hyp Data frame of hypotheses (each row is one, columns are x1,y1,x2,y2,prob,size)
#' @param n Number of hypotheses to return if there is more than one best
#' @return A vector whose length is the same number of rows as hyp. Each cell is TRUE if
#' that hypothesis is one of the best and FALSE otherwise.

returnBestHypothesis = function(hyp,n){
  
  nHyp <- nrow(hyp)
  allbest <- hyp$posterior==max(hyp$posterior)
  goodones <- rep(FALSE,nHyp)
  u <- unique(hyp$size)
  
  if (length(which(allbest))<n | length(which(allbest))<=1) {
    return(allbest)    
  } 
  
  if (n==1) {
    # select hypothesis at random
    best <- sample(which(allbest),size=1)
    goodones[best] <- TRUE
    return(goodones)
  } else {
    # get smallest hypothesis that's best
    allsmall <- allbest==TRUE & hyp$size==min(hyp$size[allbest])
    if (length(which(allsmall))>1) {
      small <- sample(which(allsmall),size=1)
    } else {
      small <- which(allsmall)
    }
    # get largest hypothesis that's best
    alllarge <- allbest==TRUE & hyp$size==max(hyp$size[allbest])
    if (length(which(alllarge))>1) {
      large <- sample(which(alllarge),size=1)
    } else {
      large <- which(alllarge)
    }
    goodones[small] <- TRUE
    goodones[large] <- TRUE
    # get more if necessary
    if (n>2) {
      allbest[small] <- FALSE
      allbest[large] <- FALSE
      rest <- sample(which(allbest),size=n-2,replace=FALSE)
      goodones[rest] <- TRUE
    }
    return(goodones)
  }

}


# ******************************
#     ruleOutHypotheses
# ******************************
#' Given a data frame with a set of points, another dataframe with the ptsXhypothesis,
#' matrix of what is consistent or not, and a third data frame with a set of observations
#' tells whether each hypothesis is consistent with all of the observations
#' @param pts Data frame of points (each row is one, columns are x and y)
#' @param consPt Data frame of pts X hypotheses (TRUE if consistent)
#' @param obs Data frame with a set of observations
#' @param whichObs Set as "all" if looking at all of them, "pos" if positive only, 
#' "neg" if negative only; default is "all"
#' @return a vector with length of the number of hypotheses
#' where TRUE indicates that that hypothesis is consistent with all of the data
#' and FALSE indicates that it is not (at least one data point is inconsistent, meaning
#' a positive one is outside it or a negative one is inside it)

ruleOutHypotheses = function(pts,consPt,obs,whichObs="all"){
  
  nPos <- sum(obs$category=="positive")
  nNeg <- sum(obs$category=="negative")
  
  # if there are no data points, return all consistent
  if (whichObs=="neg" & nNeg==0) {
    return(rep(TRUE,ncol(consPt)))
  } else if (whichObs=="pos" & nPos==0) {
    return(rep(TRUE,ncol(consPt)))
  } else if (whichObs=="all" & nNeg==0 & nPos==0) {
    return(rep(TRUE,ncol(consPt)))
  }
  
  # set names  
  pts$name <- rownames(pts)
  consPt$name <- rownames(consPt)
  
  if (whichObs!="neg" & nPos>0) {
    # first, find the positive observations
    pObs <- obs %>% filter(category=="positive")
    nPos <- nrow(pObs)
    posList <- rep(NA,nPos)
    for (i in 1:nPos) {
      temp <- pts %>% filter(x==pObs$x[i] & y==pObs$y[i])
      posList[i] <- temp$name[1]
    }
    # calculate which hypotheses are consistent
    consPos <- consPt[posList,]
    consPos$name <- NULL
    posResults <- colSums(consPos)    
  }
  
  
  # now find the negative observations
  if (whichObs!="pos" & nNeg>0) {
    nObs <- obs %>% filter(category=="negative")
    nNeg <- nrow(nObs)
    negList <- rep(NA,nNeg)
    for (i in 1:nNeg) {
      temp <- pts %>% filter(x==nObs$x[i] & y==nObs$y[i])
      negList[i] <- temp$name[1]
    }
    # calculate which hypotheses are consistent
    consNeg <- consPt[negList,]
    consNeg$name <- NULL
    negResults <- abs(nNeg-colSums(consNeg))
  }
  
  if (whichObs=="pos") {
    hypResults <- posResults==nPos
  } else if (whichObs=="neg") {
    hypResults <- negResults==nNeg
  } else {
    numObs <- nrow(obs)
    results <- negResults + posResults
    hypResults <- results==numObs    
  }
  
  return(hypResults)
}



# ******************************
#     sampleNextPoint
# ******************************
#' @title Given a probability distribution, samples the next point
#' @param consPts pts x hyp matrix with TRUE where pt is in hyp FALSE otherwise
#' @param pts Set of points, where each row is one
#' @param targetHyp True rectangle, indicated as index in hyps
#' @param probDist Probability distribution to sample from
#' @param obs Previous observations; necessary so as to not repeat (default: NA)
#' @param maximise TRUE if should choose the max prob point (default: FALSE), rather than sample from the distribution. 
#' @return a data frame where each row contains the x and y coordinates 
#' of the observation sampled and  category, as well as the index of that point
sampleNextPoint = function(consPts, pts, targetHyp, probDist, obs=NA,
                           maximise=FALSE) {
  
  # first some cleaning and tidying
  numObs <- 1
  numOldObs <- 0
  origPts <- pts
  nPts <- nrow(pts)
  nHyp <- ncol(consPts)
  pts$index <- 1:nPts
  ptsLeft <- 1:nPts

  # check to make sure it is possible to sample without replacement
  if (!is.null(nrow(obs))) {
    numOldObs <- nrow(obs)
  }
  
  if (nPts - numObs - numOldObs < 0) {
    stop("sampleNextPoint: not enough points left")      
  } 
  
  # sample proportional to that posterior
  if (maximise) {
    myPts <- pts$index[which.is.max(probDist)]
  } else {
    myPts <- sample(pts$index,size=1,prob=probDist)
  }
  
  x <- pts$x[myPts]
  y <- pts$y[myPts]
  index <- pts$index[myPts]
  name <- paste0("p",index)
  if (consPts[myPts,targetHyp]) {
    category <- "positive"
  } else {
    category <- "negative"
  }

  myObs <- data.frame(name,x,y,category,index)
  
  return(myObs)
}


# ******************************
#     updateHypotheses
# ******************************
#' @title Returns the probability distribution over hypotheses given a new point
#' @param probPts pts x hyp matrix with probabilites of each pt at each hyp
#' @param consPts pts x hyp matrix with TRUE where pt is in hyp FALSE otherwise
#' @param newPt New point to be calculated in
#' @param hyp Hypotheses to be updated
#' @return a modified hyp containing a new posterior and adding the point to it
updateHypotheses = function(probPts, consPts, newPt, hyp) {
  
  nPts <- nrow(probPts)
  nHyp <- ncol(probPts)
  
  if (newPt$category=="positive") {
    ch <- consPts[newPt$index,]
    hyp$nPos <- hyp$nPos+1
    temp <- hyp$consPos+ch
    hyp$consPos <- as.vector(temp==(hyp$nPos+1))
  } else {
    ch <- !consPts[newPt$index,]
    hyp$nNeg <- hyp$nNeg+1
    temp <- hyp$consNeg+ch
    hyp$consNeg <- as.vector(temp==(hyp$nNeg+1))
  }
  prior <- hyp$posterior
  # calculate the posterior for each hypothesis
  # likelihood is given by probPts after ruling out inconsistent hypotheses
  likelihood <- as.numeric(t(as.vector(probPts[newPt$index,]*ch)))
  if (newPt$category=="positive") {
    hyp$likePos <- hyp$likePos*likelihood
    hyp$likePos <- hyp$likePos/sum(hyp$likePos)
  } else {
    #likelihood <- t(likelihood)
    hyp$likeNeg <- hyp$likeNeg*likelihood
    hyp$likeNeg <- hyp$likeNeg/sum(hyp$likeNeg)
  }
  hyp$posterior <- likelihood*prior
  hyp$posterior <- hyp$posterior/sum(hyp$posterior)
  
  return(hyp)
  
}


# ******************************
#     updatePoints
# ******************************
#' @title Returns the probability distribution over points
#' @param probPts pts x hyp matrix with probabilites of each pt at each hyp
#' @param newPt New point to be calculated in
#' @param posterior Probabilities to multiply points by
#' @param pts Points to be updated
#' @return a modified pts containing a new posterior and adding the point to it
updatePoints = function(probPts, newPt, posterior, pts) {
  
  nPts <- nrow(probPts)
  nHyp <- ncol(probPts)
  signs <- sign(pts$posterior)

  pts$type[newPt$index] <- newPt$category
  pts$selected[newPt$index] <- TRUE
  
  # calculate new probabilities for each point
  postPts <- sweep(probPts,2,posterior,"*")
  temp <- rowSums(postPts)
  temp[newPt$index] <- 0
  pts$posterior <- (temp/sum(temp,na.rm=TRUE))*signs

  return(pts)

}

#' Set prior densities for each hypothesis 
#'
#' @param sizes Vector of sizes for each hypothesis
#' @param mean Mean of the normal distribution. Median rectangle size is 27, so that's default mean. 
#' @param sd Standard deviation of the normal distribution. 
#' @return Vector of probability densities for each hypothesis
normalPrior <- function(sizes, mean = 27, sd = 25){
  prior <-  dnorm(sizes, mean, sd)
  prior
}


getPtProbs = function(d, all_conditions, experiment, target_blocks = c(2,8), H = 10){
  nConds <- length(all_conditions[,1])
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"blocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]
    
    # Filter data based on those conditions
    data <- d %>%
      filter(cond == condition & clue == clueNum & block == b)
    
    
    # Set up grid of all possible points
    x  = seq(0.5, 9.5)
    y = seq(0.5, 9.5)
    pts = expand.grid(x,y)
    colnames(pts) = c("x","y")
    
    # get the provider helpfulness in each condition
    if (condition == "HS" | condition == "HN") {
      provider <- "helpful"
    } else if (condition == "RS" | condition == "RN") {
      provider <- "random"
    } else if (condition == "MS" | condition == "MN") {
      provider <- "misleading"
    } else if (condition == "US" | condition == "UN") {
      provider <- "uninformative"
      recursion = TRUE
    }
    
    
    # Find out how many participant responses there are
    nResp <- length(data[,1])
    
    # Empty data frame to fill with the points contained with a participant response
    ptsIn <- NULL
    
    # function to vectorise isInRect function
    applyIsInRect <- function(df, rectangle) {
      inRectangle <- apply(df[,c("x","y")], 1, function(p) isInRectangle(p, rectangle))
      return(inRectangle)
    }
    
    # Loop through each participant response, seeing which grid cells/points were contained within each response
    for (j in 1:nResp) {
      rect <- c(data[j,"x1"], data[j,"y1"], data[j,"x2"], data[j,"y2"])
      isIn <- applyIsInRect(pts, rect)
      ptsIn <- cbind(ptsIn, isIn)
    }
    # Calculate how many times a point was contained within a given response
    sums <- rowSums(ptsIn)
    
    # Convert to probability
    probs <- sums/sum(sums)
    
    # Combine into a single data frame
    pts$posterior <- probs
    ptProbs  <- pts
    
    # Save data
    if (experiment == "sim") {
      save(ptProbs, file = here(paste0("experiment-scenarios/heatmap/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    } else {
      save(ptProbs, file = here(paste0("experiment-",experiment,"/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
      
    }
    
  }
}

getHypProbs = function(d, all_conditions, experiment, target_blocks = c(2,8), H = 10, file_label = ""){
  nConds <- length(all_conditions[,1])
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"blocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]
    
    # Filter data based on those conditions
    data <- d %>%
      filter(cond == condition & clue == clueNum & block == b)
    
    # load in pre-calculated hypotheses 
    load(here("datafiles/x0to10y0to10.RData"))
    
    nHyp <- nrow(hyp)
    hyp$index = 1:nHyp
    
    likelihood = c()
    for(i in hyp$index) {
      likelihood[i] = sum(data$index == i)
    }
    
    hyp$likelihood = likelihood
    hyp$posterior = (hyp$prior*hyp$likelihood)/sum(hyp$prior*hyp$likelihood)
    
    # Save data
    if (experiment == "sim") {
      save(hyp, file = here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
    } else {
      save(hyp, file = here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,file_label,".Rdata")))
      
    }
    
  }
}

#' Find the area of overlap between two rectangles
#'
#' @param rect1 coordinates of first rectangle, e.g., c(<x1>, <y1>, <x2>, <y2>)
#' @param rect2 coordinates of second rectangle 
#'
#' @return area of overlap between the two rectangles
#'
#' @examples rectOverlap(rect1 = c(2,2,6,6), rect2 = c(4,4,8,8))
rectOverlap = function(rect1, rect2) { 
  
  rect1 = as.numeric(rect1)
  rect2 = as.numeric(rect2)
  
  # figure out which x and y values are the maximum and minimum values of each rectangle 
  x1Max = max(c(rect1[1], rect1[3]))
  x1Min = min(c(rect1[1], rect1[3]))
  y1Max = max(c(rect1[2], rect1[4]))
  y1Min = min(c(rect1[2], rect1[4]))
  
  x2Max = max(c(rect2[1], rect2[3]))
  x2Min = min(c(rect2[1], rect2[3]))
  y2Max = max(c(rect2[2], rect2[4]))
  y2Min = min(c(rect2[2], rect2[4]))
  
  # equation for calculating the area of the overlap 
  dx = min(x1Max, x2Max) - max(x1Min, x2Min)
  dy = min(y1Max, y2Max) - max(y1Min, y2Min)
  
  if (dx >= 0 & dy >= 0) {
    size = dx * dy
    prop = size/max(findSize(rect1), findSize(rect2)) # prop = proportion of overlap
  } else {
    prop = 0
  }
  return(prop)
}  
