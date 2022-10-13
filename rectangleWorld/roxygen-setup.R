root <- here()
setwd(root)
cat("Package: rectangle-world\n", file = "DESCRIPTION")
cat("Version: 1.0.0\n", file = "DESCRIPTION", append = TRUE)
library(devtools)
document()
roxygen2::roxygenize()


setwd("rectangleWorld/R/")
document()

https://users.soe.ucsc.edu/~daspence/Examples/Rpackages.html