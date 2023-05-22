#load(here(paste0("experiment-", input$experiment, "/data/derived/data_cartesian.Rdata")))

plotShinyHeatMaps = function(d, b, condition, clueNum, experiment, target_blocks = c(2,8)){
    
      # get the provider helpfulness in each condition
    if (condition == "HS" | condition == "HN") {
      provider <- "helpful"
    } else if (condition == "RS" | condition == "RN") {
      provider <- "random"
    } else if (condition == "MS" | condition == "MN") {
      provider <- "misleading"
    } else if (condition == "US" | condition == "UN") {
      provider <- "uninformative"
    }
    
    
  
    # Filter data based on those conditions
    data <- d %>%
      filter(cond == condition & clue == clueNum & block == b)
  
    # load point probabilities
    if (experiment == "sim") {
      load(here(paste0("experiment-scenarios/heatmap/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    } else {
      load(here(paste0("experiment-",experiment,"/data/derived/point-probs/pp-",condition,"-b-",b,"-c-",clueNum,".Rdata")))
    }
    
  
  # Load clues pertaining to condition
  
  # check if the block is a target block
  if (b %in% target_blocks) {
    load(here(paste0("experiment-scenarios/target-blocks/data/target-block-",b,"-Cartesian.Rdata")))
    
    # Get observations pertaining to condtition
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
    ptProbs <- rename(ptProbs, Probability = probs)
    
    
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
    
    
    # Plot heat map for that condition
    heatMap <- ptProbs %>% ggplot() +
      geom_raster(aes(x = ptProbs[,1], y = ptProbs[,2], fill = Probability))+
      scale_fill_gradientn(colours = colourScale)+
      geom_rect(aes(xmin = data[1,"ground_truth_x1"], ymin = data[1,"ground_truth_y1"], xmax = data[1,"ground_truth_x2"], ymax = data[1,"ground_truth_y2"]), alpha = 0, colour = "yellow", linetype = 4, linewidth = 1.4)+
      geom_point(data = obs, aes(x = x, y = y, colour = category), size = 7)+
      scale_colour_manual(values = c("positive" = "green", "negative" = "red"))+
      #{if ( clueNum == 1)labs(subtitle = st)} +
      guides(color = FALSE)+
      theme_void()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            text = element_text(size = 4),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            legend.position = "none")+
      labs(x = "", y = "")
    
  heatMap

}

#plotShinyHeatMaps(b = 1, condition = "US", clueNum = 1, experiment = 1)
