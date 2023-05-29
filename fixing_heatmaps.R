# plot hypothesis heat map 
tempPts <- updatePoints(posProbPts[,,zeroA],obsExp[1,],
                        posterior=lnHyp$tchD$posterior,pts=lnPts$tchD)
t <- "Learner: hypothesis heatmap"

p1Dhmap <- plotDistribution(allPts=tempPts,xrange=xrange,yrange=yrange,
                            obs=obsExp[1,],whichDist="posterior",
                            title=t, subtitle=st)