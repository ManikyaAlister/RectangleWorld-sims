obs = obs1[1,]
source("functions/optimiseAlpha.R")
grid = alphaGridSearch(borders, obs)
learn = pedLearner(borders,obs)
gridDist = alphaGridSearchDist(borders,obs)
colnames(learn)

rect = c(2,2,4,4)

match = gridDist[gridDist[,1] == 2 & gridDist[,2] == 2 & gridDist[,3] == 4 & gridDist[,4] == 4,]

a06 = gridDist[gridDist[,"alpha"] == "0.6",]
a0 = gridDist[gridDist[,"alpha"] == "0",]
a1 = gridDist[gridDist[,"alpha"] == "1",]

plot(1:length(a1[,1]), sort(a1[,"posterior"]), "l")
lines(1:length(a0[,1]), sort(a0[,"posterior"]), "l")
lines(1:length(a06[,1]), sort(a06[,"posterior"]), "l", col = "green")


plot(1:length)

