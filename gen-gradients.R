source("functions/samplingFunctions.R")
source("functions/optimiseAlpha.R")
borders = makeBorders(0:10)
rectSmall = c(2,2,4,4)
rectLarge = c(1,1,9,9)

obs = generateObs(c(1,2,3), c(0,1,2), rectSmall)

debugonce(simulatePar)
t1 = simulatePar(obs[obs[,"nObsCond"] == "1",], borders)

test = pedLearner(borders = borders, observations = obs[obs[,"nObsCond"] == "1",])

debugonce(pedLearner)

tmp = obs[obs[,"nObsCond"] == "1",]

if (is.vector(trueRects)){
  tmp1 = (as.data.frame(obs[obs[,"nObsCond"] == "1",]) )
}