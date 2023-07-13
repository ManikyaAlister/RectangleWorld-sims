#load(here(paste0("experiment-", input$experiment, "/data/derived/data_cartesian.Rdata")))
source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))



 plotShinyHeatMaps = function(b, condition, clueNum, experiment, target_blocks = c(2,8), H = 10){
    
   # get the provider helpfulness in each condition
   if (condition == "HS" | condition == "HN") {
     provider <- "helpful"
     recursion <- FALSE
   } else if (condition == "RS" | condition == "RN") {
     provider <- "random"
     recursion <- FALSE
   } else if (condition == "MS" | condition == "MN") {
     provider <- "misleading"
     recursion <- FALSE
   } else if (condition == "US" | condition == "UN") {
     provider <- "uninformative"
     recursion <- TRUE
   }
     
   # Load the pre-calculated data if not loaded already 
   if(!exists("xrange")){
     fileSeg <- paste0("x0to", H, "y0to", H)
     fn <- paste0("datafiles/", fileSeg, ".RData")
     load(here(fn)) 
   }
   
     # Load hypothesis probabilities
     if (experiment == "sim") {
       load(here(paste0("experiment-scenarios/heatmap/data/derived/hyp-probs/hp-",provider,"-b-",b,"-c-",clueNum,".Rdata")))
     } else {
       load(here(paste0("experiment-",experiment,"/data/derived/hyp-probs/hp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
       
     }
     
   # Load clues pertaining to condition
   
   # check if the block is a target block
   if (b %in% target_blocks) {
     load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",b,"-Cartesian.Rdata")))
     
     # Get observations pertaining to condition
     obs <- targetBlock$observations[1:clueNum,]
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
 
   if (experiment == "sim"){
     if(condition == "HS"){
       fullCond <- "Helpful provider (α = 1)"
     } else if(condition == "MS"){
     fullCond <- "Misleading provider (α = -1)"
     } else if (condition == "US") {
       fullCond <- "Uninformative provider (α = -1)"
     } else if(condition == "RS"){
       fullCond <- "Random provider (α = 0)"
     } 
   } else { 
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
     fullCond <- "Random, No Cover Story"}
   }
   
   colourScale <- c("white","hotpink", "navy")
   # find the index of alpha = 0
   zeroA = which.min(abs(alphas))
   
   # make index column 
   pts$index = 1:nrow(pts)
   
   # get index of the observations 
   merged_df <- merge(obs, pts, by.x = c("x", "y"), by.y = c("x", "y"))
   obs$index = merged_df$index
   
   # make "selected" column (necessary for update points)
   #ptProbs$selected = FALSE
   #ptProbs$selected[obs$index] = TRUE
   
   # plot hypothesis heat map 
   tempPts <- updatePoints(posProbPts[,,zeroA],obs[clueNum,],
                           posterior=hyp$posterior,pts=pts)
   
   heatMap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
                               obs=obs[1:clueNum,],whichDist="posterior", title = NULL, subtitle = fullCond)
 
  heatMap 
 }  
 
 
 
 
 # 
 # 
 # plotLearnerHypDistribution = function(observations,
 #                                       H = 10,
 #                                       prior = "normal",
 #                                       alpha = 1,
 #                                       nTrials = 4,
 #                                       recursion = FALSE) {
 #   # Source functions
 #   source(here("genericFunctions.R"))
 #   source(here("calculatingFunctions.R"))
 #   source(here("plottingFunctions.R"))
 #   
 #   # Load the pre-calculated data if not loaded already 
 #   if(!exists("hyp")){
 #     fileSeg <- paste0("x0to", H, "y0to", H)
 #     fn <- paste0("datafiles/", fileSeg, ".RData")
 #     load(here(fn)) 
 #   }
 #   
 #   # Recursive learner
 #   if (recursion == TRUE) {
 #     fileSeg <- paste0("x0to", H, "y0to", H)
 #     # learner assumes teacher is helpful (and teacher knows)
 #     if (alpha > 0 & alpha < 1) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveLow.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("H", str_replace(alpha, "0.", "0"))
 #     } else if (alpha == 1) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("H", alpha)
 #     } else if (alpha >= 2) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveHigh.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("H", alpha)
 #       # learner assumes teacher is weak/random (and teacher knows)
 #     } else if (alpha == 0) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
 #       load(here(fn))
 #       recursionLevel <- "W"
 #     } else if (alpha == -1) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveMain.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("D", abs(alpha))
 #       # learner assumes teacher is deceptive (and teacher knows)
 #     } else if (alpha < 0 & alpha > -1) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveLow.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("D", str_replace(alpha, "-0.", "0"))
 #     } else if (alpha <= -2) {
 #       fn <- paste0("datafiles/", fileSeg, "recursiveHigh.RData")
 #       load(here(fn))
 #       recursionLevel <- paste0("D", abs(alpha))
 #     }
 #     # rename recursive all prob points array so it is generic
 #     recursionFile <- paste0(recursionLevel, "allProbPts")
 #     allProbPts <- get(recursionFile)
 #   }
 #   
 #   
 #   
 #   # Create indexing column for points (necessary for updating hypotheses)
 #   pts$index = 1:length(pts[, 1])
 #   
 #   #  Set scenario parameters  ---------------------------------------------
 #   
 #   
 #   # All hypotheses tracked by the learner
 #   lnHyp <- hyp
 #   
 #   # set initial prior over hypotheses
 #   if (prior == "normal") {
 #     lnHyp$prior <- normalPrior(hyp$size)
 #   }
 #   
 #   # prior is just the posterior from the last trial
 #   lnHyp$posterior <- lnHyp$prior
 #   
 #   # Create an index column that keeps track of each hypothesis, so it's easier to compare them
 #   lnHyp$index <- 1:length(lnHyp[, 1])
 #   
 #   
 #   # set alphas based on function input
 #   lA <- which(alphas == alpha)
 #   zeroA <- which(alphas == 0)
 #   lnAlphaText <- returnAlpha(alpha)
 #   
 #   # Set up empty data structured to be filled each trial
 #   heatMapList = list()
 #   obs = NULL
 #   
 #   for (i in 1:nTrials) {
 #     # Teacher samples new point
 #     newPt <- observations[i,]
 #     # Find the index of that point in the pre-calculated points (necessary for probability functions)
 #     newPt$index <-
 #       pts[newPt[, 1] == pts[, 1] & newPt[, 2] == pts[, 2], "index"]
 #     # Combine this new point with any points that have been sampled previously
 #     obs <- rbind(obs, newPt)
 #     obs <- obs %>% filter(!is.na(x))
 #     # learner updates their estimate of the hypotheses, given the point that was generated
 #     lnHyp <-
 #       updateHypotheses(allProbPts[, , lA], consPts, newPt, lnHyp)
 #     
 #     # plot hypothesis heat map 
 #     tempPts <- updatePoints(posProbPts[,,zeroA],obs[i,],
 #                             posterior=lnHyp$posterior,pts=pts)
 #     
 #     heatMap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
 #                                 obs=obs[1:i,],whichDist="posterior", title = NULL, subtitle = NULL)
 #     
 #     
 #     # Record the learner distribution over hypotheses for this trial
 #     heatMapList[[i]] <- heatMap
 #   }
 #   
 #   heatMapList
 # }
 # 
 # 
