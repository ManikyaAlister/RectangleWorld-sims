
##############################

# Generate Experiment Block #

#############################
library(here)


# Set up   --------------------------------------------------------

# Source functions
source(here("genericFunctions.R"))
source(here("calculatingFunctions.R"))
source(here("plottingFunctions.R"))

# Load the pre-calculated data 
fileSeg <- paste0("x0to",H,"y0to",H)
fn <- paste0("datafiles/",fileSeg,".RData")
load(here(fn))

#  Set block parameters  ---------------------------------------------

# Set X and Y range for rectangle grid
H <- 10
# All possible rectangles
borders <- makeBorders(0:H, 0:H)
# Size of the true rectangle
trueRectSize <- "medium"
# Teacher's alpha
tchAlpha <- 1
# Alpha the teacher thinks the learner thinks the teacher has
tchLnAlpha <- 1
# All points tracked by the teacher 
tchPts <- pts
# All hypotheses tracked by the teacher 
tchHyp <- hyp
# Number of trials in a block/points provided by the teacher
nTrials <- 4
# Draw the index of a rectangle from all possible rectangles.
trueHNum <- createRectangle(borders, trueRectSize)
# Coordinates of rectangle
trueH <- getCoordinates(borders, trueHNum)
# Number of best hypotheses plotted 
nBestH <- 3

# Determine whether the teacher is choosing the best point or sampling proportional to distribution
maximise <- TRUE

obs <- NA

# set alphas based on parameters above
tA <- which(alphas==tchAlpha)
tchAlphaText <- returnAlpha(tchAlpha)
tlA <- which(alphas==tchLnAlpha)
tchLnAlphaText <- returnAlpha(tchLnAlpha)

# Make directory to save this scenario in -------------------------------
dirname <- paste0(trueRectSize, "-rect-",trueHNum,"-tch-alpha-",tchAlpha)
dir.create(here(paste0("experiment-scenarios/points/",dirname,"/")))
dir.create(here(paste0("experiment-scenarios/points/",dirname,"/figures/")))
dir.create(here(paste0("experiment-scenarios/points/",dirname,"/data/")))




# Generate points by the teacher ------------------------------------------

# set initial prior: prior is just the posterior from the last prior
tchHyp$posterior <- tchHyp$prior

# empty vecotr to fill with the names of plots 
plots <- NULL
for (i in 1:nTrials) {
# first step: the teacher generates a sampling distribution over points
tchPts$posterior <- getSamplingDistribution(allProbPts[,,tlA],consPts,tchPts,
                                            trueHNum,priors=tchHyp$posterior,alpha=tchAlpha) 
t <- paste0("Teacher sampling distribution: alpha=",tchAlpha)
# pt1step1 <- plotColourfulDistribution(allPts=tchPts,xrange=xrange,yrange=yrange,
#                                       trueRectangle=trueH,obs=obs,
#                                       title=t, subtitle=st)

# step two: teacher samples the next point based on that distribution
newPt <- sampleNextPoint(consPts,pts,trueHNum,abs(tchPts$posterior),obs=obs,
                         maximise=maximise)
obs <- rbind(obs,newPt)
obs <- obs %>% filter(!is.na(x))

# step three: teacher updates their estimate of the learner's distribution over hypotheses, given the point that was generated
tchHyp <- updateHypotheses(allProbPts[,,tlA],consPts,newPt,tchHyp)
bestH <- returnBestHypothesis(tchHyp,n=nBestH)
t <- "Teacher's chosen points + hypothesis distribution for learner"
st <- paste0("Teacher's alpha = ", tchAlpha)
step3 <- plotHypotheses(trueH,obs,tchHyp[bestH,],xrange=xrange,
                           yrange=yrange,title=t, subtitle = st)

plots[i] <- assign(paste0("pt",i,"step3"), step3)

}

blockScenario <- ggarrange(pt1step3, pt2step3,pt3step3,pt4step3)


# Save plots and data -----------------------------------------------------

# plots 
ggsave(blockScenario, filename = here(paste0("experiment-scenarios/points/",dirname,"/figures/fig-",trueHNum,"a",tchAlpha,".png")), height = 10, width = 10)

# data
save(obs, file = here(paste0("experiment-scenarios/points/",dirname,"/data/obs-",trueHNum,"a",tchAlpha,".Rdata")))
