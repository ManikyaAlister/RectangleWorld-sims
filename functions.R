# sets up all possible rectangles within a given hypothesis space 
makeBorders = function(range){
  
  # Create array with all possible rectangle coordinates within the range 
  borders = expand.grid(range,range,range,range)
  
  for (i in 1:length(borders[,1])){ # replace duplicate rectangles with NA
    if (borders[i,1]-borders[i,3] > 0 | borders[i,2]-borders[i,4] > 0){
      borders[i,] = NA
    }
  }
  
  borders = borders[complete.cases(borders),] # delete any rows with NA (rows that previously held duplicate rectangles)
  
  # replace rows where size = 0 with NA
  for (i in 1:length(borders[,1])){
    if (borders[i,1] == borders[i,3] | borders[i,2] == borders[i,4]) {
      borders[i,] = NA 
    }
  }
  
  borders = borders[complete.cases(borders),] # delete any rows with NA (rows that previously held rectangles with size 0) 
  
  return(borders)   
  
}

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
      obs[i, "category"] = "positive"
    } else {
      obs[i, "category"] = "negative"
    }
  }
  return(obs)
}


# **** Given the observations, how likely are different hypotheses? 
# Weak Sampling ~ All hypotheses that contain the observation are equally as likely
# In our scenario it's not quite that simple, because the hypothesis also cannot contain negative evidence 

areInCat = function(borders, observations, catLabel = "positive") {
  # function that tells you whether certain observations fall within a rectangle
  # borders = array of hypothesised category boundary points (coordinates of rectangles)
  # observations = points that have been labelled as belonging to a certain category
  # catLabel = positive evidence that you're checking falls within a certain hypothesis space/category boundary/rectangle
  
  if (is.vector(observations)) {
    observations = observations[observations["category"] == catLabel]
  } else {
    observations = observations[observations[, "category"] == catLabel, ]
  }
  
  
  if (length(observations) == 0) {
    return(NA) # No observations of that category
  } else {
    if (is.vector(observations)) {
      # figures out if there's only 1 observation
      nObs = 1
    } else {
      nObs = length(observations[, 1]) # if multiple observations, how many?
    }
    
    isInCat = array(dim = c(length(borders[, 1]), nObs)) # set up empty array to fill with whether a hypothesized category contains each observation (is a valid hypothesis)
    
    if (nObs == 1) {
      # if there's only one observation, check whether each hypothesis (rectangle) contains the observation
      f = function(borders) {
        isInRectangle(r = borders,  p = as.numeric(observations[c("x", "y")]))
      }
      isInCat = apply(borders, 1, f)
    } else {
      # if there are multiple observations, loop through each observation and do above for each
      f = function(borders) {
        isInRectangle(r = borders, p = as.numeric(c(observations[i, "x"], observations[i, "y"])))
      }
      for (i in 1:nObs) {
        isInCat[, i] = apply(borders, 1, f)
      }
    }
    return(isInCat)
    
  }
  
  
  
}

# function to find the size of each rectangle (necessary for strong sampling)
findSize <- function (r) {
  (abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}

findSizeNeg = function(r, range = 100) { # apply to all hypotheses that are inconsistent
  range-(abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}

