rm(list = ls())
library(here)
library(patchwork)

load(here("experiment-3/data/derived/data_priors_cartesian.Rdata"))
source(here("plottingFunctions.R"))
source(here("calculatingFunctions.R"))
filter_priors = function(data, block) {
  b <- block
  prior <- data %>%
    filter(block %in% b) %>%
    select(x1, y1, x2, y2, size_resp, index)
  prior
}

prior_b8 <- filter_priors(d_priors_cartesian, 8) %>%
  mutate(size = size_resp,
         Prior = "Clue 0, Block 8")
prior_all <- filter_priors(d_priors_cartesian, 1:8)

# get the full spectrum of possible hypotheses
load(here("datafiles/x0to10y0to10.RData"))

prior_hist = function(data, hyp) {
  
  # rename column for consistency with data
  hyp$size_resp <- hyp$size
  
  hyp_freq <- hyp %>%
    group_by(size_resp) %>%
    summarise(drawn_count = n()) %>%
    mutate(size_resp = as.numeric(size_resp))
  
  hyp %>%
    ggplot(aes(x = size_resp)) +
    geom_bar(data = data)
    #geom_bar(colour = "blue")
    #geom_line(data = hyp_freq, aes(x = size_resp, y = drawn_count))
}

scaled_rectangle_sizes <- function(data, hyp) {
  # Rename column for consistency with data
  hyp$size_resp <- as.character(hyp$size)
  
  # Calculate the frequency of each drawn rectangle size
  drawn_rect_freq <- data %>%
    group_by(size_resp) %>%
    summarise(drawn_count = n())
  
  hyp_freq <- hyp %>%
    group_by(size_resp) %>%
    summarise(drawn_count = n()) %>%
    mutate(size_resp = as.numeric(size_resp))
  
  # Merge drawn rectangle frequencies with hyp data to get possible rectangle counts
  combined_data <- merge(drawn_rect_freq, hyp_freq, by.x = "size_resp", by.y = "size_resp", all.x = TRUE)
  
  # Calculate the scaled frequencies
  combined_data$scaled_freq <- combined_data$drawn_count.x / combined_data$drawn_count.y
  combined_data
}

plotPriorHistScaled = function(scaled_data, scaled = TRUE){
  # Plot histogram
  ggplot(scaled_data, aes(x = size_resp, y = scaled_freq)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(x = "Rectangle Size", y = "Scaled Frequency", title = "Histogram of Scaled Rectangles")
}

b8_scaled <- scaled_rectangle_sizes(prior_b8, hyp)
plotPriorHistScaled(b8_scaled)

b8_scaled %>% ggplot(aes(x = size_resp, weight = scaled_freq)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Size Response", y = "Density") +
  theme_minimal()



# histogram of prior size
prior_hist(prior_b8, hyp)

prior_hist(prior_all, hyp)

sizeNormalDist = function(size_resp, label){
  m <- mean(size_resp)
  sd <- sd(size_resp)
  # Generate a sequence of values covering the range of the data
  x_values <- seq(0, 100, length.out = 100)
  # Calculate the density of the normal distribution for these values
  dens <- round(dnorm(x_values, mean = m, sd = sd),3)
  
  d <- cbind(x_values, dens, label, m, sd)
  d
}

count_sizes <- hyp %>%
  group_by(size) %>%
  summarise(count = n())


normalDistPrior <- sizeNormalDist(hyp$size, "Flat Prior")
c0b8DistPrior <- sizeNormalDist(prior_b8$size_resp, "Clue 0, Block 8")

# Bind the hyp and prior_b8 data frames, adding a Prior column
d_combined_size <- bind_rows(
  hyp %>% mutate(Prior = "Flat Prior"),
 prior_b8 %>% mutate(Prior = "Observed Prior"),
  # repeat prior_b8 so that we can compare a scaled version with an unscaled version
  prior_b8 %>% mutate(Prior = "Observed Prior (Scaled)") 
)

# Add the 'count' column to the 'prior_b8' dataframe by joining with 'hyp'
d_combined_size <- d_combined_size %>%
  left_join(count_sizes, by = "size")


# d_combined_size %>%
#   group_by(size, Prior) %>%
#   summarise(n()/count)

# Now plot the density, using count as weights for the flat prior
density_plot <- d_combined_size %>%
  ggplot(aes(x = size, fill = Prior, weight = ifelse(Prior == "Observed Prior (Scaled)", count, 1))) +
  geom_density(alpha = 0.5, adjust = 1) +
  labs(x = "Rectangle Size", y = "Density") +
  scale_fill_manual(values = c("Observed Prior" = "red", "Flat Prior" = "blue", "Observed Prior (Scaled)" = "green")) +
  theme_bw() +
  theme(legend.position = "bottom")

density_plot


getPropInRect = function(rectangles) {
  # for each point, find the number of rectangles that contain that point divided by the number of possible rectangles
  sum_points <- apply(pts[,1:2],1,function(x) sum(isInRectangle(x, rectangles)))/nrow(rectangles)
  sum_points/sum(sum_points)
}


# get the number of rectangles drawn in the block 8 priors that contain each point 
sum_points_b8 <- getPropInRect(prior_b8[,1:4])

sum_points_all <- getPropInRect(prior_all[,1:4])


# get the number of points that you would expect if every combination of rectangles was drawn.
scale_points <-getPropInRect(hyp[,1:4])


prior_pt_probs <- pts %>%
  mutate(hyp_points_all = sum_points_all,
         hyp_points_b8 = sum_points_b8,
         scale = scale_points,
         posterior =sum_points_all/scale_points)

plotHypotheses(r = prior_b8[,1:4],title = "", subtitle = "Priors for block 8")
plotHypotheses(r = prior_all[,1:4],title = "", subtitle = "Priors for all blocks")

clue_0_b8 <- ggplot(prior_pt_probs, aes(x = x, y = y, fill = hyp_points_b8)) +
  geom_tile() +
  labs(title = "Observed Prior in Block 8", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

prior_dist <- ggplot(prior_pt_probs, aes(x = x, y = y, fill = scale_points)) +
  geom_tile() +
  labs(title = "Flat Prior Distribution Over Size", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

clue_0 <- ggplot(prior_pt_probs, aes(x = x, y = y, fill = sum_points_all)) +
  geom_tile() +
  labs(title = "Rectangles Drawn at Clue Zero", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")




(prior_dist+clue_0_b8)/density_plot

ggsave(filename = here("experiment-3/modelling/05_plots/priors.png"), width = 14, height = 10
       )

