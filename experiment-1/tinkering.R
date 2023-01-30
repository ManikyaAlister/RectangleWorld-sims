library(dplyr)
library(here)

load(here("experiment-1/data/derived/data_cartesian.Rdata"))
load(here("experiment-scenarios/target-blocks/data/target-block-2-Cartesian.Rdata"))
source(here("getLearnerHypDistributions.R"))

# What should the hypothesis distributions be for each alpha?
alphas = c(-5,-2,-1,-0.5,-0.1, 0, 0.1, 0.5, 1, 2, 5)
obs8 = targetBlock$observations
t = 2
for (i in 1:length(alphas)){
  alpha = alphas[i]
  dist = getLearnerHypDistribution(obs2, alpha = alpha, prior = "flat")
  save(dist, file = here(paste0("experiment-scenarios/hypothesis-distributions/t-",t,"-dist-alpha_",alpha,".Rdata")))
}

alphas = posteriors %>%
  group_by(alphas) %>%
  sum = sum(posterior)

tm = filter(posteriors, index == 1065) 

which.max(tm$posterior[1:11])

tmp5 = filter(posteriors, alpha == 1) %>%
  group_by(index)%>%
  summarise(sum = sum(posterior))%>%
  arrange(sum) 

tmp6 = filter(posteriors, alpha == 0)%>%
  group_by(index)%>%
  summarise(sum(posterior))%>%
  arrange(sum)

tmp7 = filter(posteriors, alpha == -1)%>%
  group_by(index)%>%
  summarise(sum = sum(posterior)) %>%
  arrange(sum)

cbnd = cbind(tmp5, tmp7)

plot(1:length(tmp5$sum), tmp5$sum,"l")
lines(1:length(tmp5$sum), tmp7$sum,"l", col = "green")

sum(tmp5$sum)

sum(tmp7$sum)









tmp = posteriors %>%
  group_by(alpha) %>%
  summarise(sum(posterior))
  
max(posteriors$posterior)

posteriors[posteriors[,"index"]==1065,]

tmp1 = getLearnerHypDistribution(targetBlock$observations, alpha = 1, prior = "flat")
m1 = sum(tmp1[[3]]$posterior)
length(tmp1[[3]]$posterior)
min(tmp1[[3]]$posterior)



tmp5 = getLearnerHypDistribution(targetBlock$observations, alpha = 5, prior = "flat")
m5 = sum(tmp1[[3]]$posterior)
length(tmp5[[3]]$posterior)
min(tmp5[[3]]$posterior)

df5$logPost = log(df5$posterior)

df5 = as.data.frame(tmp5[[3]]) %>%
  arrange(posterior)

df1 = as.data.frame(tmp1[[3]]) %>%
  arrange(posterior)

plot(1:length(df5[,1]),df5$posterior, "l")
lines(1:length(df1[,1]),df1$posterior, "l", col = "green")

plot(tmp1[,"size"], tmp1[,"posterior"])

