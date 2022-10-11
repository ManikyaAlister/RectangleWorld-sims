#### Pedagogical teacher 

# 1. set up true rectangle 
cat1 = c(2,2,6,8)
trueRectangle = cat1
# 2. set up all possible points
range = 1:10
plot(test)

pedTeacher = function(trueRectangle, range){
  allPoints = expand.grid(range,range) # set up all possible points
  # label points as either positive or negative evidence
  f = function(p) isInRectangle(r = trueRectangle, p = p) # alter isInRectangle function to be appropriate for apply() (include true rectangle)
  positiveEvidence = apply(allPoints,1,f) # tells us if a point is in the rectangle or not
  category = rep(NA,length(allPoints)) # empty column to fill with labels
  observations = cbind(allPoints,category) # join into 1 matrix
  
  for (i in 1:length(positiveEvidence)){
    if (positiveEvidence[i] == TRUE){
      observations[i,"category"] = "positive"
    } else {
      observations[i,"category"] = "negative"
    }
  }
  
  # Calculate the learner's likelihood:
  # see which points maximize the pedagogical learner's posterior probability that the true rectangle is the best rectangle relative to the other rectangle
  
  for (i in 1:length(allPoints)){
    observation = pedLearner(trueRectangle,observations[i,])
  }
  
  learner = pedLearner(trueRectangle,observations)
  
  
  return(learner)
}



learnerLik = function(observations,trueRecatngle,numPos = FALSE,numNeg = FALSE){
  likelihood = c()
  for(i in 1:length(observations[,1])){
    if (observations[i,"category"] == "positive") {
      likelihood[i] = (1 / findSize(trueRectangle) ^ numPos)
    } else {
      likelihood[i] = 1 / ((length(range)*length(range))-(findSize(trueRectangle)) ^ numPos)
    }
  }
  return(likelihood)
}


test = pedTeacher(as.matrix(cat1),range)

test = pedLearner(cat1,obs1Pos)
test2 = pedLik(observations,trueRectangle,numPos = 3,numNeg = 3)
