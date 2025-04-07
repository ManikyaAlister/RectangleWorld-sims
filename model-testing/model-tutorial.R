source(here("functions/generic-functions.R"))
source(here("functions/calculatingFunctions.R"))

# create hypothesis space
H <- 2
x <- 0:H
y <- 0:H
pts_x <- seq(0.5,H-0.5, 1)
pts_y <- pts_x
pts <- expand.grid(x = pts_x, y = pts_y)

# define all possible hypotheses using make Boarders function, which removes all rectangles of length 0 and adds other Bayesian info. 
hyp <- makeBorders(x,y) 
hyp$rect_id <- rownames(hyp)
justHyp <- hyp[,1:4]

ggplot() +
  geom_rect(data = hyp, fill = "lightblue", color = "black", alpha = 0.7,
            aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) +
  #geom_point(data = pts, aes(x = x, y = y)) +
  scale_x_continuous(breaks = 0:2, minor_breaks = 0:2) +
  scale_y_continuous(breaks = 0:2, minor_breaks = 0:2) +
  coord_fixed() +
  facet_wrap(~ rect_id) +
  theme_minimal() +
  labs(title = "All possible hypotheses", x = "X", y = "Y")

# define alphas
tchAlpha <- 1 # teachers intentions
tchLnAlpha <- 1 # what the teacher believes the learner thinks the teacher's intentions are 
lnTchAlpha <- 1 # what the learner actually believes the teachers intentions are

# before seeing any points, what does the learner think is  the probability of each point given each hypothesis ( p(d|h) )? 
# That is, for each hypothesis, what points the learner thinks the provider should be most likely to draw if that hypothesis were true.

# need to calculate this separately for positive points and negative points
ppp <- findProbabilityOfPoints(hyp,pts, "pos", alpha = tchAlpha) # likelihood of positive points
nnn <- findProbabilityOfPoints(hyp,pts, "neg", alpha = tchAlpha) # same for negative
aaa <- ppp + nnn # combine to have an overall probability of each  type of point given a hypothesis. 


# to do next: heat map of points 





sizes <- apply(hyp, 1, findSize)



findSize(hyp[,1])


plotHypotheses(hyp[1,1:4],xrange = x, yrange = y, )

geom_rect(mapping = aes())
