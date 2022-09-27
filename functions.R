

# Function to figure out whether a certain observation is within a category
isInRectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}

# Observations sampled and *teacher* categorizes -- exemplars/observations chosen via Weak Sampling
# practical problem with this function is that it doesn't guarantee any observations of either type of evidence
weakSampler = function(nObs){
  
  obs = array(dim = c(nObs,3)) # array to fill with observation coordinates and their category
  colnames(obs) = c("x","y","category")
  
  for (i in 1:nObs) {
    obs[i, 1] = sample(range, 1, replace = TRUE)
    obs[i, 2] = sample(range, 1, replace = TRUE)
    if (isInRectangle(obs[i, 1:2], cat1)) {
      obs[i, "category"] = "1"
    } else {
      obs[i, "category"] = "none"
    }
  }
  return(obs)
}


# **** Given the observations, how likely are different hypotheses? 
# Weak Sampling ~ All hypotheses that contain the observation are equally as likely
# In our scenario it's not quite that simple, because the hypothesis also cannot contain negative evidence 

areInCat = function(borders,observations,catLabel) { # function that finds out which observations fall within a hypothesized category boundary (rectangle)
  # borders = array of hypothesised category boundary points (coordinates of rectangles)
  # observations = points that have been labelled as belonging to a certain category 
  # catLabel = the category that you're checking falls within a certain hypothesis space/category boundary/rectangle
  observations = observations[observations[,"category"]==catLabel,]
  
  if(length(observations) == 0) { # stop if there are no observations with specified catLabel
    stop("There are no observations with that category label")
  }
  
  if (is.vector(observations)){ # figures out if there's only 1 observation
    nObs = 1
  } else {
    nObs = length(observations[,1]) # if multiple observations, how many?
  }
  
  isInCat = array(dim = c(length(borders[, 1]), nObs)) # set up empty array to fill with whether a hypothesized category contains each observation
  
  if (nObs == 1) { # if there's only one observation, check whether each hypothesis (rectangle) contains the observation
    f = function(borders) isInRectangle(r = borders,  p = as.numeric(observations[c("x","y")]))
    isInCat = apply(borders,1,f)
  } else { # if there are multiple observations, loop through each observation and do above for each
    f = function(borders) isInRectangle(r = borders, p = as.numeric(c(observations[i,"x"],observations[i,"y"])))
    for (i in 1:nObs) { 
      isInCat[, i] = apply(borders,1,f)
    }
  }
  return(isInCat)
}

# weakLearner function updates the likelihood of each hypothesis given weak sampling
weakLearner = function(borders, observations) {
  isInRectPos = areInCat(borders, observations, "1")
  areInRect = function(isInRect)
    setequal(isInRect, rep(TRUE, length(as.data.frame(isInRect))))
  positiveEvidence = if (length(as.data.frame(isInRectPos)) > 1) {
    # tells us which rectangles contain all of the observations of the category of interest
    apply(isInRectPos, 1, areInRect)
  } else {
    # if length is 1, no need to do the above function
    positiveEvidence = isInRectPos
  }
  
  isInRectNeg = areInCat(borders, observations, "none") # does the hypothesised rectangle contain negative evidence?
  negativeEvidence = if (length(as.data.frame(isInRectPos)) > 1) {
    negativeEvidence = rowSums(isInRectNeg) > 0
  } else {
    negativeEvidence = isInRectNeg
  }
  
  hypotheses = cbind(borders, positiveEvidence, negativeEvidence)
  hypotheses = hypotheses[hypotheses[, "positiveEvidence"] == TRUE &
                            hypotheses[, "negativeEvidence"] == FALSE, ]
  hypotheses$likelihood = 1
  hypotheses$posterior = hypotheses[,"likelihood"]/sum(hypotheses[,"likelihood"]) # no prior yet so posterior is just likelihood over sum(likelihood)
  return(hypotheses)
}


# function for plotting the generalization gradients of a weak learner 
plotWeak = function(hypotheses,observations, categoryBoundary, range = 1:10) {
  plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
  rect(hypotheses[,1],hypotheses[,2],hypotheses[,3],hypotheses[,4], col= rgb(0,0,1,alpha=hypotheses[,"posterior"]),lwd = 0.01)
                                                                             #) # making alpha equal t 1/the number of hypotheses makes the transparency of the plot equivilent to the relitive probability of each hypothesis. 
  points(observations)
  rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "darkblue", lwd = 3)
}


# function to find the size of each rectangle (necessary for strong sampling)
findSize <- function (r) {
  (abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}

# Strong sampling -- weights each hypothesis by its size, such that smaller hypotheses are allocated a higher probability
# ~ 1/(|h|^n), where |h| = size of the hypothesis, and n = number of observations
strongLearner = function(borders, observations) {
  weak = weakLearner(borders, observations) # builds off of the weak learner because it's just one extra step
  logLikelihood = log(1 / findSize(weak) ^ length(observations[, 1])) # operationalising n, number of observations, as both positive and negative observations
  likelihood = (1 / findSize(weak) ^ length(observations[, 1])) / sum(1 /
                                                                        findSize(weak) ^ length(observations[, 1])) # non-log probability so that I can integrate it with alpha/transparency when plotting
  posterior = likelihood/sum(likelihood)
  strongHypotheses = cbind(weak, likelihood, logLikelihood,posterior)
  colnames(strongHypotheses) = c(
    "x1",
    "y1",
    "x2",
    "y2",
    "positiveEvidence",
    "negativeEvidence",
    "likelihood",
    "logLikelihood",
    "posterior"
  )
  return(strongHypotheses)
}

# function for plotting the generalization gradients of a strong learner 
plotStrong = function(strongHypotheses, observations, categoryBoundary, range = 1:10){
  plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
  rect(strongHypotheses[,1],strongHypotheses[,2],strongHypotheses[,3],strongHypotheses[,4], col= rgb(0,0,1.0,alpha=strongHypotheses[,"posterior"]), lwd = 0.01) # making alpha equivalent to the likelihood to show strong sampling gradient 
  points(observations)
  rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "darkblue", lwd = 3)
}

pedegogicalLearner = function(strongHypotheses){
  strongHypotheses$best = strongHypotheses[,"posterior"] == max(strongHypotheses["posterior"])
  pedHypotheses = strongHypotheses[strongHypotheses[,"best"]==TRUE,]
  pedHypotheses$posterior = 1
  return(pedHypotheses)
}
  

plotPed = function(pedHypotheses, observations, categoryBoundary, range = 1:10){
  plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
  rect(pedHypotheses[,1],pedHypotheses[,2],pedHypotheses[,3],pedHypotheses[,4], col= rgb(0,0,1.0,alpha=pedHypotheses[,"posterior"]), lwd = 0.01) # making alpha equivalent to the likelihood to show strong sampling gradient 
  points(observations)
  rect(categoryBoundary[1],categoryBoundary[2],categoryBoundary[3],categoryBoundary[4],border = "darkblue", lwd = 3)
}

alphaPedegogicalLearner = function(borders, observations, alphaParam) {
    weak = weakLearner(borders, observations) # builds off of the weak learner because it's just one extra step
    #logLikelihood = alphaParam * log(1 / findSize(weak) ^ length(observations[, 1])) # operationalising n, number of observations, as both positive and negative observations
    likelihood = (1 / findSize(weak) ^ length(observations[, 1]))^alphaParam# non-log probability so that I can integrate it with alpha when plotting. 
    posterior = likelihood/sum(likelihood)
    # (cont. from above): I'm dividing them by the sum so that the generalization gradients show up a bit better in the plots. By summing these the likelihood acts more like a posterior probability, but since I'm not working with a prior at the moment it's basically the same. 
    pedHypotheses = cbind(weak[,1:6], likelihood,posterior)
    colnames(pedHypotheses) = c(
      "x1",
      "y1",
      "x2",
      "y2",
      "positiveEvidence",
      "negativeEvidence",
      "likelihood",
      #"logLikelihood",
      "posterior"
    )
    return(pedHypotheses)
}


