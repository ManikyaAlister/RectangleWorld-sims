# load libraries
library(here)

# source functions
source(here("genericFunctions.R"))

# Generate rectangles for each block of the experiment  ---------------------

## create set of all possible rectangles 
borders <- makeBorders()

## Define rectangle sizes for each block 
rectangleSizes <- c("medium", "medium", "small", "large", "medium", "large", "small", "large", "small","medium")

## create rectangles
rectangles <- createMultiRectangles(borders, rectangleSizes)

## set scenario details for saving
scenario <- paste(substr( rectangleSizes , start = 1 , stop = 1 ), collapse = "")
version <- 1

## save rectangles for later
save(rectangles, file = here(paste0("experimentScenarios/rectangles/",scenario,"-V",version,".Rdata")))


# Generate points for each block of the experiment ------------------------

## Set experiment parameters



