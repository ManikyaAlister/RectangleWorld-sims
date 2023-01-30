library(jsonlite)
library(here)

conds = c("HS", "RS", "US", "MS", "HN", "RN", "UN", "MN")
all_conds = rep(conds, 100)
tids = 0:length(all_conds)

array = list()
for (i in 1:length(all_conds)){
  array[[i]] = list("tid" = paste0("tid_",tids[i]), "learn"= list("cond" = all_conds[i]))
}

#colnames(array) = c("tid", "")

json <- toJSON(array, pretty = TRUE, auto_unbox = TRUE)
write(json, file = here("experiment-2/mturk/tids.json"))
