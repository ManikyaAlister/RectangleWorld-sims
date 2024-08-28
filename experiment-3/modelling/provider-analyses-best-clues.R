rm(list = ls())
library(here)
library(tidyverse)
source(here("calculatingFunctions.R"))

# Set up experiment parameters
H <- 10

# Load the pre-calculated data
fileSeg <- paste0("x0to", H, "y0to", H)
fn <- paste0("datafiles/", fileSeg, ".RData")
load(here(fn))

# Establish true rectangles and their indexes
rectangles <- list(
  "medium" = c(4, 1, 9, 4),
  "small" = c(1, 6, 4, 8),
  "large" = c(0, 1, 9, 9)
)

# define teacher models
models <- c("helpful", "misleading", "uninformative", "random")

# point posteriors empty data frame 
point_posteriors <- NULL
plot_list <- list()
# loop through each model and rectangels
for (i in 1:length(models)) {
  for (j in 1:length(rectangles)) {
    
    teacher_model <- models[i]
    
    # Establish tacher and learner alphas in each condition
    if (teacher_model == "helpful") {
      tchAlpha = 1
      lnAlpha = 1
      tchLnAlpha = 1
    } else if (teacher_model == "misleading") {
      # misleading naive
      tchAlpha = -1
      lnAlpha = 1
      tchLnAlpha = 1
    } else if (teacher_model == "random") {
      tchAlpha = 0
      lnAlpha = 0
      tchLnAlpha = 0
    } else if (teacher_model == "uninformative") {
      # misleading aware
      tchAlpha = -1
      lnAlpha = -1
      tchLnAlpha = -1
    }
    
    # No existing observations
    obs <- NULL
    
    tlA <- which(alphas == tchLnAlpha)
    
    # rectangle size
    rectangle_size <- names(rectangles)[j]
    
    # rectangle coordinates
    rectangle_coords <- rectangles[j]
    
    # Index of rectangle
    trueHNum <- getRectangleIndex(rectangle_coords, nRectangles = 1)
    
    # get posteriors of points
    posterior <-
      getSamplingDistribution(
        allProbPts[, , tlA],
        consPts,
        pts,
        trueHNum,
        priors = 1/nrow(pts),
        alpha = tchAlpha,
        obs = obs)
    
    # add whether the point is positive or negtative 
    point_type <- ifelse(posterior > 0, "positive", "negative")    
    
    combination_name <- paste0(rectangle_size, "-", teacher_model)
    
    # combine into data frame
    posteriors_df <-
      data.frame(teacher_model,
                 rectangle_size,
                 point = 1:100,
                 posterior = abs(posterior), #abs() because negative points are represented as negative posterior (was done this way originally for plotting)
                 point_type)

    plot <- posteriors_df %>%
      arrange(posterior) %>%
      mutate(point = factor(point, levels = point[order(posterior)]))%>%
      ggplot(aes(x = point, y = posterior, colour = point_type, shape = point_type))+
      geom_point(size = 1)+
      lims(y = c(0,0.12))+
      labs(title= combination_name)+
      scale_color_manual(values = c("orange", "darkgreen"))+
      #scale_colour_viridis_d()+
      theme_bw()+
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
    
    plot_list[[combination_name]] <- plot
    
    #  add to list
    point_posteriors <- rbind(point_posteriors, posteriors_df)
    
  }
}

ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE)

# load participant data 
load(here("experiment-3/data/clean/data_teaching_cartesian.Rdata"))

# add index column to points df
pts$index <- 1:100


# adjust point posteriors so the index is no longer a factor (only needed that for plotting)
point_posteriors_adj <- point_posteriors %>%
  mutate(point = as.integer(point))

# Join d_cartesian_t with pts to get the corresponding point index for each response
d_cartesian_with_index <- d_cartesian_t %>%
  left_join(pts, by = c("response_x1" = "x", "response_y1" = "y")) %>%
  select(1:11, clue,index)
  
  d_cartesian_with_posteriors <- d_cartesian_with_index %>%   # remove unnecessary info
    left_join(point_posteriors_adj, by = c("index" = "point", "size" = "rectangle_size"))  # add posteriors of each point according to different conditions

# get summary statistics indicating how well each model performed in each condition
model_performance <- d_cartesian_with_posteriors %>%
  group_by(provider_cond, teacher_model) %>%
  summarise(posterior = median(posterior))

model_performance %>%
  ggplot(aes(x = teacher_model,y = posterior, fill = teacher_model)) +
  geom_col()+
  facet_wrap(~provider_cond)

d_cartesian_with_posteriors %>%
  group_by(pid,teacher_model, provider_cond)%>%
  summarise(posterior = median(posterior)) %>% # adjust so each observation is a single participant
  mutate(teacher_model = case_when(
    teacher_model == "helpful" ~ "Helpful",
    teacher_model == "misleading" ~ str_wrap("Misleading Naive", width = 10),
    teacher_model == "uninformative" ~ str_wrap("Misleading Aware", width = 10),
    teacher_model == "random" ~ "Random"
  ),
  provider_cond = case_when(
    provider_cond == "helpful" ~ "Helpful",
    provider_cond == "misleading" ~ "Misleading Naive",
    provider_cond == "uninformative" ~ "Misleading Aware"
  )) %>%
  #filter(clue == 2) %>%
  ggplot(aes(x = teacher_model,y = posterior, fill = teacher_model)) +
  geom_jitter(aes(colour = teacher_model),alpha = .2)+
  geom_boxplot(outliers = FALSE, colour = "black", alpha = .8)+
  #geom_col(data = model_performance, colour = "black")+
  scale_fill_brewer(type = "qual")+
  scale_colour_brewer(type = "qual")+
  facet_wrap(~provider_cond)+
  theme_bw()+
  labs(title = "All at Once (best points at trial 0)", fill = "Provider Model", colour = "Provider Model", x = "Provider Model", y = "Posterior")+
  theme(legend.position =  "none", axis.text.x = element_text(angle = 45, hjust = 1))

point_posteriors %>%
  group_by(teacher_model)%>%
  summarise(mean = median(posterior), median = median(posterior))

point_posteriors %>%
  ggplot(aes(x = teacher_model,y = posterior, fill = teacher_model)) +
  geom_jitter(aes(colour = teacher_model), alpha = .05)+
  #geom_boxplot(outliers = FALSE)+
  #geom_violin()+
  geom_col(data = model_performance, colour = "black")+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  #facet_wrap(~provider_cond)

d_cartesian_with_posteriors %>%
  #filter(trial_index %in% c(3,4)) %>%
  ggplot(aes(x = teacher_model,y = posterior, fill = teacher_model)) +
  geom_jitter(aes(colour = teacher_model),alpha = .05)+
  geom_boxplot(outliers = FALSE)+
  #geom_col(data = model_performance, colour = "black")+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  facet_wrap(~provider_cond+size, ncol = 3)

# point_posteriors %>%
#   ggplot(aes(x = point, y = posterior, colour = point_type))+
#   geom_point()+
#   facet_wrap(~teacher_model+rectangle_size, scales = "free_x")+
#   theme(axis.text.x = element_blank())

# Calculate the probability of each point as per empirical data
d_cartesian_t %>%
  #filter(clue == 1) %>%
  group_by(response_x1, response_y1, provider_cond, size, ground_truth_x1, ground_truth_x2, ground_truth_y1, ground_truth_y2) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(provider_cond, size) %>%
  mutate(probability = count / sum(count)) %>%  # Convert counts to probabilities within each group
  ungroup() %>%

# Generate the heat map of provider resoibses
ggplot(aes(x = response_x1, y = response_y1, fill = probability)) +
  geom_tile() +
  facet_grid(provider_cond ~ size) +  # Create separate heatmaps for each provider_cond and size
  scale_fill_gradient(low = "white", high = "red") +  # Color scale for the heatmap
  geom_rect(
            aes(xmin = ground_truth_x1, xmax = ground_truth_x2,
                ymin = ground_truth_y1, ymax = ground_truth_y2),
            #inherit.aes = FALSE,  # To prevent using the x, y, fill mappings
            color = "purple", 
            fill = NA,
            linetype = "dashed")+  # Add dashed border for checkered effect

  labs(title = "Heatmap of Participant Responses in the Provider Phase",
       x = "",
       y = "",
       fill = "Probability") +  # Update label to "Probability"
  theme_minimal() +
  #theme(axis.text = element_blank()) +
  coord_fixed()  # Ensure squares are of equal size


true_rectangles <- d_cartesian_t %>%
  group_by(size, ground_truth_x1,ground_truth_y1, ground_truth_x2, ground_truth_y2)%>%
  summarise(size = unique(size))

# do the same with model predictions 
posterior_with_coords <- point_posteriors %>%
  mutate(point = as.integer(point)) %>%
  left_join(select(pts,x,y,index), by = c("point" = "index")) %>%
  left_join(true_rectangles, by = c("rectangle_size" = "size"))

posterior_with_coords %>% 
  ggplot(aes(x = x, y = y, fill = posterior)) +
  geom_tile() +
  facet_grid(teacher_model ~ rectangle_size) +  # Create separate heatmaps for each provider_cond and size
  scale_fill_gradient(low = "white", high = "red") +  # Color scale for the heatmap
  geom_rect(
    aes(xmin = ground_truth_x1, xmax = ground_truth_x2,
        ymin = ground_truth_y1, ymax = ground_truth_y2),
    #inherit.aes = FALSE,  # To prevent using the x, y, fill mappings
    color = "purple", 
    fill = NA,
    linetype = "dashed")+  # Add dashed border for checkered effect
  
  labs(title = "Heatmap of Model Predictions in the Provider Phase",
       subtitle = " 'All at once' Method",
       x = "",
       y = "",
       fill = "Probability") +  # Update label to "Probability"
  theme_minimal() +
  theme(axis.text = element_blank()) +
  coord_fixed()  # Ensure squares are of equal siz
  