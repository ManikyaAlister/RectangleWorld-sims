borders = makeBorders(0:10)
grid = alphaGridSearch(borders,observations)

# small rectangle 
smallRect = c(2,2,4,4)



# 1 positive 1 negative, small
obs11small = generateObs(1,1,smallRect)

# grid search
gridSmall11 = alphaGridSearch(borders, obs11small)

# trusting participant
a1Small11 = simulatePar(obs11small, borders, alpha = 1)

# weak sampling assumptions participant 
a0Small11 = simulatePar(obs11small, borders, alpha = 0)

# suspicious participant
aNeg1Small11 = simulatePar(obs11small, borders, alpha = -1)

# estimate alphas
a1Small11Fit = fitAlpha(a1Small11, gridSmall11)
plot(a1Small11Fit[,"alpha"], a1Small11Fit[,"prob"])
lines(a1Small11Fit[,"alpha"], a1Small11Fit[,"prob"])

a0Small11Fit = fitAlpha(a0Small11, gridSmall11)
plot(a0Small11Fit[,"alpha"], a0Small11Fit[,"prob"])
lines(a0Small11Fit[,"alpha"], a0Small11Fit[,"prob"])

aNeg1Small11Fit = fitAlpha(aNeg1Small11, gridSmall11)
plot(aNeg1Small11Fit[,"alpha"], aNeg1Small11Fit[,"prob"])
barplot(x = aNeg1Small11Fit[,"alpha"], y = aNeg1Small11Fit[,"prob"])


# 2 positive 2 negative
obs22small = generateObs(2,2,smallRect)


# large rectangle
largeRect = c(2,2,8,8)

# 1 positive 1 negative
obs11large = generateObs(1,1,largeRect)

# 2 positive 2 negative
obs22large = generateObs(2,2,largeRect)




trRec = genTrueRects(1,borders)
observations = generateObs(1,1,trRec)
pa1 = simulatePar(observations, borders, alpha = 1)
a1Grid = alphaGridSearch(borders,observations)
eligible = fitAlpha(pa1, a1Grid)
#eligibleAlphas = rectOverlap(a1Grid[,1:4], pa1)
  

# need to calculate p(choosing that rectangle | has that alpha ) or maybe other way around. 
  #a1Grid[a1Grid[,c("x1", "y1", "x2", "y2")] == pa1]

pa04 = simulatePar(observations, borders, alpha = 0.4)

eligible = fitAlpha(pa04, a1Grid)
