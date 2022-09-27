
# Range of features
range = 1:10

# Array with all possible rectangle coordinates within the range 
borders = expand.grid(range,range,range,range)

# Define true category regions
cat1 = c(2,2,4,6)
cat2 = c(6,4,8,8)

# visualize true category regions
plot(c(1,max(range)), c(1, max(range)), type= "n", xlab = "", ylab = "")
rect(cat1[1],cat1[2],cat1[3],cat1[4],border = "blue")
rect(cat2[1],cat2[2],cat2[3],cat2[4],border = "red")


# Function to figure out whether a certain observation is within a category
isInRectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}

# Observations sampled and *teacher* categorizes -- exemplars/observations chosen via Weak Sampling
weakSampler = function(nObs){
  
obs = array(dim = c(nObs,3)) # array to fill with observation coordinates and their category
colnames(obs) = c("x","y","category")

for (i in 1:nObs) {
  obs[i, 1] = sample(range, 1, replace = TRUE)
  obs[i, 2] = sample(range, 1, replace = TRUE)
  if (isInRectangle(obs[i, 1:2], cat1)) {
    obs[i, "category"] = "1"
  } else if (isInRectangle(obs[i, 1:2], cat2)) {
    obs[i, "category"] = "2"
  } else {
    obs[i, "category"] = "none"
  }
}
return(obs)
}

# nObs = 10
# obs = weakSampler(10)
# points(obs) # plot observations (need to have previously called the plot above)

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
    isInCat = isInRectangle(borders, p = as.numeric(observations[c("x","y")]))
  } else { # if there are multiple observations, loop through each observation and do above for each
  for (i in 1:nObs) { 
    isInCat[, i] = isInRectangle(borders, p = c(observations[i,"x"],observations[i,"y"]))
    }
  }
  return(isInCat)
}

##### Problem I'm Having is Below -- I don't think isInRectangle function is working. At the very least, hypotheses that are bwing
# classified as containing all three observations are not correct 

isInRect1 = areInCat(borders,obs,"1")
areInRect = function(isInRect) setequal(isInRect,rep(TRUE,length(as.data.frame(isInRect)))) 
positiveEvidence1 = apply(isInRect1,1,areInRect) # tells us which rectangles contain all of the observations of the category of interest

rect(positiveEvidence1[,1],positiveEvidence1[,2],positiveEvidence1[,3],positiveEvidence1[,4], col= rgb(0,0,1.0,alpha=0.05))


isInRect2 = areInCat(borders,obs,"2")
positiveEvidence2 = apply(isInRect2,1,areInRect)

isInRectNone = areInCat(borders,obs,"none")
negativeEvidence = rowSums(isInRectNone > 0) # if there is more than 1 true, it means that the hypothesised rectangle contains negative evidence

tmp1 = apply(isInRectNone,1,areInRect)
tmpFilter = cbind(borders,tmp1)
tmpFiltered = tmpFilter[tmpFilter[,"tmp1"]==TRUE,]
rect(tmpFiltered[,1],tmpFiltered[,2],tmpFiltered[,3],tmpFiltered[,4])


filterBorders1 = cbind(borders,positiveEvidence1,positiveEvidence2)
filteredBorders1 = filterBorders1[filterBorders1[,"positiveEvidence1"]==TRUE & filterBorders1[,"positiveEvidence2"]==FALSE,]

rect(filteredBorders1[,1],filteredBorders1[,2],filteredBorders1[,3],filteredBorders1[,4], col= rgb(0,0,1.0,alpha=0.05))

filterBorders2 = cbind(borders,positiveEvidence2,positiveEvidence1)
filteredBorders2 = filterBorders2[filterBorders2[,"positiveEvidence2"]==TRUE & filterBorders2[,"positiveEvidence1"]==FALSE,]

rect(filteredBorders2[,1],filteredBorders2[,2],filteredBorders2[,3],filteredBorders2[,4], col= rgb(1.0,0,0,alpha=0.05))


tmp = areInCat(rbind(c(1,3,2,4),c(8,8,10,10)),obs,"none")

testNT = filterBorders2[filterBorders2[,"negativeEvidence"]==TRUE,]
testNF = filterBorders2[filterBorders2[,"negativeEvidence"]==FALSE,]


plotRectangle(filteredBorders2)

rect(filteredBorders2[1,1],filteredBorders2[1,2],filteredBorders2[1,3],filteredBorders2[1,4], col= rgb(0,0,1.0,alpha=0.05))
rect(filteredBorders2[,1],filteredBorders2[,2],filteredBorders2[,3],filteredBorders2[,4], col= rgb(0,0,1.0,alpha=0.05))


for (i in 1:length(testNF[,1])){
  rect(testNF[i,1],testNF[i,2],testNF[i,3],testNF[i,4], col= rgb(0,0,1.0,alpha=0.05))
}


isInRectNone = areInCat(borders,obs,"none")
negativeEvidence = rowSums(isInRectNone) > 0 

filterBorders = cbind(borders,positiveEvidence,negativeEvidence)
filteredBorders = filterBorders[filterBorders[,"positiveEvidence"]==TRUE & filterBorders[,"negativeEvidence"]==FALSE,]

test = filterBorders[filterBorders[,"positiveEvidence"]==TRUE,]

probCat = function(positiveEvidence,negativeEvidence) { # function that figures out the probability of different rectangles sizes
# positive evidence = hypotheses that contain all the positive evidence   
# negative evidence = hypotheses that contain any the negative evidence (examples that are *not* part of the category)
}

tmp = isInRectangle(c(8,7),borders)
tmp1 = cbind(borders,tmp)
tmp2 = tmp1[tmp1[,5]==TRUE,]

isInCat2 = areInCat(borders, obs,"2")
isInCatNone = areInCat(borders, obs,"none")



cat1obs == TRUE & cat2obs == FALSE

compHyp = cbind(borders,probCat1) # add column saying whether each hypothesis is compatible with the data
compHype = test[test[,"probCat1"]==T,] # filter data set with only compatible hypotheses




sample(range,2)

# function to find the size of each rectangle
findSize <- function (r) {
  return ((r[2]-r[1]+1)*(r[4]-r[3]+1))
}


# Function to asily draw a rectangle by subsetting from a data frame of coordingates (boraders)
plotRectangle = function(boarders) rect(borders[,1],borders[,2],borders[,3],borders[,4], col= rgb(0,1,0,alpha=0.05) )





plotRectangle(borders,55)

findSize(borders,20) 

plot.new()




rect(1,1,5,5)

#**** FINDSIZE: calculates the size of a rectangle R
findSize <- function (r) {
  return ((r[2]-r[1]+1)*(r[4]-r[3]+1))
}



rectangle <- (function(){
  
  sideSize <- 10
  sLevel <- 100
  #spPer <- 4  # sparsity of true hypothesis in percent (needs to be changed here and in run.Rmd)
  #sp <- floor(spPer*worldSize^2/100)  # sparsity in pixels
  spRange <- floor(sLevel*sideSize^2/100) # set sparsity range to be all hypotheses with sLevel% or less sparsity
  
  # set of all one dimensional intervals
  ns <- sideSize*(sideSize+1) / 2
  spans <- matrix(nrow=ns,ncol=2)
  i <- 0
  for( left in 1:sideSize) {
    for( right in left:sideSize ) {
      i <- i+1
      spans[i,] <- c(left,right)
    }
  }
  
  # set of all rectangles
  temphyp <- matrix( nrow=ns^2, ncol=4 )
  i <- 0
  for( s in 1:ns ) {
    temphyp[i+(1:ns),] <- cbind( t(replicate(ns, spans[s,])),spans)
    i <- i+ns
  }  
  hypotheses <- temphyp[which(apply(temphyp,1,findSize)<=spRange,arr.ind=TRUE),]
  return(hypotheses)
  
})()
