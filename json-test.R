library(jsonlite)
library(here)
rm(list = ls())
#load("~/Documents/Projects/RectangleWorld/RectangleWorld-sims/experiment-scenarios/good-scenarios/data/exploritory-trialJson-t1.Rdata")

nTrials <- 4
filepath <- "experiment-scenarios/chosen-scenarios/data/1-645-tchA1/1-645-tchA1trial-obs.Rdata"
load(here(filepath))

trueRect <- as.data.frame(t(expData$trueRect))
colnames(trueRect) = c("x1","y1","x2","y2")
# convert2json = function(block, teacherCond, filepath){
#   
# }


blockid <- "b01_helpful"
block_data <- list(tid = "tid_0", blockid = blockid, trueRect = trueRect, learn = list(trialConfig = list()))
H <- 10

for ( i in 1:nTrials){
  rownames(expData$trialJson[[i]]) = NULL
  byRows <- list()
  obsDf <- expData$trialJson[[i]]
  for(j in 1:H){
    byRows[[j]] <-  obsDf[obsDf[,"row"] == j,]
  }
  
  
  #load(here(filepath))
  #rownames(trialJson) <- NULL
  block_data$learn$trialConfig[[i]] <- list(id = paste0("tc",i-1), 
                                            grid = list(
                                              width = as.numeric(H),
                                              height = H,
                                              cells = byRows,
                                              trialType = as.character(paste0("t0",i)),
                                              xref = paste0("testxref-00",i)
                  
                                            )
                                            )
}


block_data_JSON <- toJSON(block_data, pretty = TRUE, auto_unbox = TRUE)
#save(block_data_JSON, file = here("experiment-scenarios/good-scenarios/data/block_trial_test.json"))

write(block_data_JSON, here(paste0("experiment-scenarios/good-scenarios/data/",blockid,".json")))
#test <- toJSON(trialJson, pretty = TRUE)
#test2 <- toJSON(trialObs, pretty = TRUE)
