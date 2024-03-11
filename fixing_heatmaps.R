# source(here("calculatingFunctions.R"))
# source(here("plottingFunctions.R"))
# 
# # configure
# 
# #experiment <- 1
# b <- 8
# clueNum <- 1
# condition <- "HS"
# lA <- 1
# zeroA <- 6
# 
# # load probabilities 
# load(here(paste0("experiment-scenarios/heatmap/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
# colnames(ptProbs) <- c("x", "y", "posterior")
# 
# # load observations
# load(here("datafiles/obsExp.RData"))
# 
# # Load initial points and hypotheses
# lnPts <- pts
# lnHyp <- hyp
# 
# # get prior of points
# lnPtProbs <- sweep(allProbPts[,,lA],2,lnHyp$prior,"*")
# lnPts$prior <- rowSums(lnPtProbs)/sum(lnPtProbs)
# 
# # plot hypothesis heat map 
# tempPts <- updatePoints(posProbPts[,,zeroA],obsExp[1,],
#                         posterior=ptProbs$posterior,pts=pts)
# 
# p1Dhmap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
#                             obs=obsExp[1,],whichDist="posterior")
# 

# New heat map function ---------------------------------------------------

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
    load(here(paste0("experiment-scenarios/heatmap/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
  } else {
    load(here(paste0("experiment-",experiment,"/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    
  }
  
}
}

plotHeatMaps = function(all_conditions, experiment, target_blocks = c(2,8), zeroA = 6, H = 10){

  nConds <- length(all_conditions[,1])
  # upload pre-calculated positive point probabilities for an alpha of zero (just for plotting clarity)
  load(here("datafiles/x0to10y0to10.RData"))
  
  # Loop through each condition, creating a separate heat map for each
  for (condId in 1:nConds){
    # Get condition data (block, conditions, clue number)
    b <- all_conditions[condId,"blocks"]
    condition <- all_conditions[condId,"conditions"]
    clueNum <- all_conditions[condId,"clues"]

    # Load data
    if (experiment == "sim") {
      load(here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    } else {
      load(here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
      
    }
    
    
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
    
    # Load clues pertaining to condition
    
    # check if the block is a target block
    if (b %in% target_blocks) {
      load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",b,"-Cartesian.Rdata")))
      
      # Get observations pertaining to condition
      obs <- targetBlock$observations[1:clueNum,]
      trueRect <- targetBlock$groundTruth
    } else {
      # find folder that contains the block data for that condition
      
      # get initial directory
      directory <- "experiment-scenarios/hand-picked-blocks/data"
      
      # Pattern to match the folder name
      pattern <- paste0(".*",b,"-.*",provider)
      
      # Find folders matching the pattern in the directory
      matching_directory <- list.files(directory, pattern = pattern, full.names = TRUE)
      
      # Check if any matching files were found
      if (length(matching_directory) > 0) {
        
        # now load actual data files within directory
        directory <- matching_directory
        
        # Pattern to match file name
        pattern <- paste0(".*\\b", b, "-\\d+-", provider, ".*\\.Rdata")
        #pattern <- paste0(".*\\b",b,".*",provider,".*\\.Rdata") 
        
        # Find files matching the pattern in the directory
        matching_files <- list.files(directory, pattern = pattern, full.names = TRUE)
        
        
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
      obs <- blockData$observations[1:clueNum,]
      obs$category <- obs$observed
      
      # convert from experiment grid format to Cartesian format 
      obs$x <- obs$x - 0.5
      obs$y <- 10-(obs$y - 0.5)
    }
    
    # Rename columns for plot legend
    #colnames(ptProbs) <- c("x", "y", "posterior")
    
    
    if(condition == "HS"){
      fullCond <- "Helpful, Cover Story"
    } else if (condition == "HN") {
      fullCond <- "Helpful, No Cover Story"
    } else if(condition == "MS"){
      fullCond <- "Misleading, Cover Story"
    } else if (condition == "MN") {
      fullCond <- "Misleading, No Cover Story"
    } else if(condition == "US"){
      fullCond <- "Uninformative, Cover Story"
    } else if (condition == "UN") {
      fullCond <- "Uninformative, No Cover Story"
    } else if(condition == "RS"){
      fullCond <- "Random, Cover Story"
    } else if (condition == "RN") {
      fullCond <- "Helpful, No Cover Story"}
    
    #st = fullCond
    
    #colourScale <- c("white","lightpink", "hotpink","lightblue","blue","navyblue")
    colourScale <- c("white","hotpink", "navy")
    #colourScale <- c("navy","hotpink")
    
    
    
    # find the index of alpha = 0
    zeroA = which.min(abs(alphas))
    
    
    # plot hypothesis heat map 
    tempPts <- updatePoints(posProbPts[,,zeroA],obs[1:clueNum,],
                            posterior=hyp$posterior,pts=pts)

    heatMap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
                                obs=obs[1:clueNum,],whichDist="posterior", title = NULL, subtitle = NULL, trueRectangle = trueRect)
    
    
    if (experiment == "sim"){
      ggsave(filename = here(paste0("experiment-scenarios/heatmap/plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)

    } else {
      ggsave(filename = here(paste0("experiment-",experiment,"/modelling/05_plots/heatmap-",condition,"-b-",b,"-c-",clueNum,".png")), width = 5, height = 5, plot = heatMap)
    }

    # Save plot to list 
    
    # track progress in console
    print(paste0(condId," out of ", nConds))
  }}
