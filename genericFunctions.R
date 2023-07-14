library(dplyr)

getRectangleIndex = function(rectangle, hyp = NULL, nRectangles, H = 10) {
  if (is.null(hyp)){
      fileSeg <- paste0("x0to", H, "y0to", H)
      fn <- paste0("datafiles/", fileSeg, ".RData")
      load(here(fn))
  }
  rectanglesIndex <- c()
  hyp$index = 1:length(hyp[,1])
  for (i in 1:nRectangles) {
    if (is.data.frame(rectangle)) {
      rect <- as.vector(as.matrix(rectangle[i,c("x1","y1","x2","y2")]))
    } else if(is.list(rectangle)){
      rect <- rectangle[[i]]
    } else if (is.vector(rectangle)) {
      rect <- rectangle
    } 
    index <-
      hyp[hyp[, "x1"] == rect[1] &
            hyp[, "y1"] == rect[2] &
            hyp[, "x2"] == rect[3] & hyp[, "y2"] == rect[4],]["index"]
    index <- as.numeric(index)
    rectanglesIndex <- c(rectanglesIndex, index)
    
  }
  rectanglesIndex
}

# ************************
#     createRectangle
# ************************
#' Returns a rectangle of size {small, medium, or large} given 
#' @param hyp The set of all hypotheses
#' @param size should be "small", "medium", or "large". default: small
#' @return a number indicating the row of hyp with the appropriate rectangle

createRectangle = function(hyp,size="small"){
  
  # figure out what size of rectangle
  sizes <- unique(hyp$size)
  sizes <- sizes[sizes < max(sizes)]
  x <- split(sizes,sort(sizes%%4))
  
  if (size=="small") {
    ss <- x[[1]]
  } else if (size=="medium") {
    ss <- x[[2]]
  } else {
    ss <- x[[4]]
  }
  hs <- hyp$size %in% ss
  h <- sample(which(hs),size=1)
  return(h)
}

# ************************
#     createMultiRectangle
# ************************
#' Returns a set of rectangles of size {small, medium, or large} given 
#' @param hyp The set of all hypotheses
#' @param size A vector of the sizes you want the rectangles to be. Should be "small", "medium", or "large". default: small
#' @return a vector indicating the rows of hyp with the appropriate rectangle
createMultiRectangles = function(hyp, sizes) {
  rectangles <- NULL
  for (i in 1:length(sizes)) {
    rectBlock <- createRectangle(hyp, sizes[i])
    rectangles[i] <- rectBlock
  }
rectangles  
}



# **********************
#     findAllPoints
# **********************
#' Returns a data frame with all of the points in a hypothesis space given by xrange and yrange
#' Assumes that each cell has one point. By convention each point is in the centre
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @return A dataframe with two columns, x and y, where each row is a unique point
findAllPoints = function(xrange=0:10,yrange=0:10){
  lowx <- min(xrange)
  highx <- max(xrange)-1
  lowy <- min(yrange)
  highy <- max(yrange)-1
  pts <- floor(expand.grid(lowx:highx,lowy:highy))+0.5

  allPts <- data.frame(pts)
  colnames(allPts) <- c("x","y")
  rownames(allPts) <- paste0("p",1:nrow(pts))
  
  return(allPts)
}


# **********************
#     findSizeNeg
# **********************
#' Function to find the size of the space outside the rectangle (necessary for pedagogical sampling)
#' Does not check to make sure the rectangle is fully within the range (assumes it is)
#' 
#' @param r vector of length 4. Coordinates of the rectangle
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
findSizeNeg = function(r, range = 0:10) { # apply to all hypotheses that are inconsistent
  (length(xrange)-1)*(length(yrange)-1)-(abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}


# **********************
#     findSize
# **********************
#' Function to find the size of each rectangle (necessary for strong sampling)
#' 
#' @param r vector of length 4. Coordinates of the rectangle
findSize <- function (r) {
  (abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}



# ************************
#     getCoordinates
# ************************
#' Returns the coordinates of the rectangle given by hypNum
#' @param hyp The set of all hypotheses
#' @param hypNum The number of the hypothesis you want the rectangle of
#' @return a vector with four columns indicating coordinates (x1,y1,x2,y2)

getCoordinates = function(hyp,hypNum){
  rect <- c(hyp$x1[hypNum],hyp$y1[hypNum],hyp$x2[hypNum],hyp$y2[hypNum])
  return(rect)
}




# **********************
#     isInRectangle
# **********************
#' Function to figure out whether a certain observation is within a rectangle
#' @param p vector of length 2. The point that you want to know is inside the rectangle or not
#' @param r vector of length 4. Coordinates of the rectangle (x1,y1,x2,y2)
#' @returns TRUE if in the rectangle, false if not
isInRectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}



# **********************
#     makeBorders
# **********************
#' Sets up all possible rectangles within a given hypothesis space 
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @return A dataframe where each row is one rectangle, 
#' the column num is a unique identifier for each hypothesis
#' the columns (x1,y1,x2,y2) are the coordinates 
#' the column prior indicates the prior probability of that hypothesis (initially equal)
#' nPos and nNeg will track the number of points of each, right now 0
#' likePos and likeNeg are the likelihood of each of those things, right now 1
#' consPos and consNeg are TRUE if the hypothsis is consistent with the pos/neg data
#' posterior is the posterior, right now equal to prior
makeBorders = function(xrange=0:10,yrange=0:10){
  
  # Create array with all possible rectangle coordinates within the range 
  borders = expand.grid(xrange,yrange,xrange,yrange)
  # get full size
  fullSize <- (length(xrange)-1)*(length(yrange)-1)
  
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
  
  nHyp <- nrow(borders)
  size <- rep(0,nHyp)
  for (i in 1:nHyp) {
    size[i] <- findSize(c(borders[i,1],borders[i,2],borders[i,3],borders[i,4]))
  }
  negSize <- fullSize - size
  prior <- rep(1,nHyp)/nHyp
  posterior <- prior
  nPos <- rep(0,nHyp)
  nNeg <- rep(0,nHyp)
  likePos <- rep(1,nHyp)
  likeNeg <- rep(1,nHyp)
  consPos <- rep(TRUE,nHyp)
  consNeg <- rep(TRUE,nHyp)
  b <- data.frame(borders,size,negSize,prior,
                  nPos,consPos,likePos,nNeg,consNeg,likeNeg,posterior)
  colnames(b) <- c("x1","y1","x2","y2","size","negSize",
                   "prior","nPos","consPos","likePos","nNeg",
                   "consNeg","likeNeg","posterior")
  rownames(b) <- paste0("h",1:nrow(b))
  return(b)   
}



# **********************
#     returnAlpha
# **********************
#' Given a number, returns a name for that alpha
#' @param alpha A number representing alpha (default: 0)
#' @return One of the following words: "W" for weak (if alpha is 
#' between 0.1 and -0.1); "BH" for a bit helpful (if alpha >= 0.1 
#' and < 0.5); "BD" for a bit deceptive (if alpha =< -0.1 and 
#' > -0.5); "H" for helpful (if alpha >=0.5 and < 1.5); 
#' "D" for deceptive (if alpha <= -0.5 and > -1.5); "VH" for 
#' very helpful (if alpha >= 1.5 and < 3); "VD" for very deceptive (if
#' alpha <= -1.5 and > -3); "EH" for extremely helpful (alpha >=3);
#' ED for extremely deceptive (alpha <= -3). 
returnAlpha = function(alpha=0){
  
  myAlpha <- NA
  if (alpha < 0.1 & alpha > -0.1) {
    myAlpha <- "w"
  } else if (alpha >= 0.1 & alpha < 0.5) {
    myAlpha <- "bh"
  } else if (alpha <= -0.1 & alpha > -0.5) {
    myAlpha <- "bd"
  } else if (alpha >= 0.5 & alpha < 1.5) {
    myAlpha <- "h"
  } else if (alpha <= -0.5 & alpha > -1.5) {
    myAlpha <- "d"
  } else if (alpha >= 1.5 & alpha < 3) {
    myAlpha <- "vh"
  } else if (alpha <= -1.5 & alpha > -3) {
    myAlpha <- "vd"
  } else if (alpha >= 3) {
    myAlpha <- "eh"
  } else {
    myAlpha <= "ed"
  }
  
  return(myAlpha)
}

loadExperimentObs = function(b, clueNum, target_blocks = c(2, 8), provider) {
  if (b %in% target_blocks) {
    load(here(
      paste0(
        "experiment-scenarios/target-blocks/data/target-block-",
        b,
        "-Cartesian.Rdata"
      )
    ))
    
    # Get observations pertaining to condition
    obs <- targetBlock$observations[1:clueNum, ]
  } else {
    # find folder that contains the block data for that condition
    
    # get initial directory
    directory <- "experiment-scenarios/hand-picked-blocks/data"
    
    # Pattern to match the folder name
    pattern <- paste0(".*", b, "-.*", provider)
    
    # Find folders matching the pattern in the directory
    matching_directory <-
      list.files(directory, pattern = pattern, full.names = TRUE)
    
    # Check if any matching files were found
    if (length(matching_directory) > 0) {
      # now load actual data files within directory
      directory <- matching_directory
      
      # Pattern to match file name
      pattern <- paste0(".*\\b", b, "-\\d+-", provider, ".*\\.Rdata")
      #pattern <- paste0(".*\\b",b,".*",provider,".*\\.Rdata")
      
      # Find files matching the pattern in the directory
      matching_files <-
        list.files(directory, pattern = pattern, full.names = TRUE)
      
      
      if (length(matching_files) > 0) {
        # Load the first matching file
        load(here(matching_files[1]))
        
      } else {
        print("no file matching specified directory")
      }
      
    } else {
      # if no matching files, print error
      print("no folder matching specified directory")
    }
    # Get observations pertaining to condtition
    obs <- blockData$observations[1:clueNum, ]
    obs$category <- obs$observed
    
    # convert from experiment grid format to Cartesian format
    obs$x <- obs$x - 0.5
    obs$y <- 10 - (obs$y - 0.5)
  }
  obs
}
