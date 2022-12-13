## Test pedagogical model 
load("obsTest.Rdata")
borders = makeBorders(0:10)

obs1 = obs[obs[,"nObsCond"] == 1,] 
obs2 = rbind(c(x = 2.5, y = 2.5,category = "positive"), c(x = 8.5, y = 8.5, category = "negative"))

l1 = pedLearner(borders, obs1)

debugonce(pedLearner)
l2 = pedLearner(borders, obs2)

plot(obs2, xlim = c(0,10), ylim = c(0,10))
rect(1,1,2,2)

