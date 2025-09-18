model_alphas <- c(-1,0,1) # alphas we want for the helpful, misleading naive, random models
recursive_alpha <- -1 # alpha that we want for the recursive (misleading aware/uninformative) model
all_plotting <- NULL 
plot_clue <- 3
plot_block <- 2
filter_cover_check <- FALSE # if filtering based on the cover story verification quiz, indicate the exclusion number

for (exp in 1:3){
  # load recursive posteriors
  
  # load posteriors for participant data
  load(here(
    paste0(
      "experiment-",
      exp,
      "/modelling/04_output/tb",
      plot_block,
      "-all-alpha-posteriors-recursive.Rdata"
    )
  ))
  
  
  # filter based on number of attemprs to pass verification question # match n cover check to posterior data
  
  # generic function
  filterCoverCheck = function(posteriors = all_alpha_posteriors, experiment = exp, filter_threshold = filter_cover_check){
    # load participant data
    load(here(
    paste0(
      "experiment-",
      exp,
      "/data/clean/clean_data.Rdata"
    )))

    # get only n_check and pid
    d_check <- data %>%
      distinct(pid, n_cover_check)

    posteriors <- posteriors %>%
      left_join(d_check, by = "pid")#, relationship = "many-to-many")
    
    filtered_posteriors <- posteriors %>%
      filter(n_cover_check < filter_threshold) # remove participants who took more than X attempts 
    
    filtered_posteriors
    }
  
  # do the filtering for the recursive cond if condition met
  if(filter_cover_check){
    all_alpha_posteriors <- filterCoverCheck()
  }
  
  recursive_raw_plotting <- all_alpha_posteriors %>%
    filter(alpha == -1, clue == plot_clue) %>%
    mutate(model = "Misleading\n Aware")
  
  # load non-recursive posteriors 
  
  # load posteriors for participant data
  load(here(
    paste0(
      "experiment-",
      exp,
      "/modelling/04_output/tb",
      plot_block,
      "-all-alpha-posteriors.Rdata"
    )
  ))

  # do the filtering for the non-recursive cond if condition met
  if(filter_cover_check){
    all_alpha_posteriors <- filterCoverCheck()
  }
  
  raw_plotting <- all_alpha_posteriors %>%
    filter(alpha %in% model_alphas, clue == plot_clue) %>%
    mutate(model = case_when(
      alpha == -1 ~ "Misleading\n Naive",
      alpha == 0 ~"Random",
      alpha == 1 ~ "Helpful"
    ))
  
  all_plotting_exp <- bind_rows(recursive_raw_plotting, raw_plotting) %>%
    mutate(experiment = paste0("Exp. ",exp))
  
  all_plotting <- bind_rows(all_plotting, all_plotting_exp)
  
}

# get the probability of the random model: all posteriors should be the same
random_prob = unique(all_plotting[all_plotting[,"model"] == "Random","posterior"])
if(length(random_prob) > 1) {
  warning("More than one value for the randm model. If you are only looking at one clue, this could be a problem")
  random_prob <- mean(random_prob) # if random prob is more than one number, it's probably because I'm looking at more than one clue, in which case just average. 
}

models <- c("Helpful", "Misleading\n Naive", "Misleading\n Aware", "Random")

# give conditions the full model names
all_plotting <- all_plotting %>%
  mutate(
  full_cond = case_when(
      cond == "HS" | cond == "HN" ~ models[1],
      cond == "MS" | cond == "MN" ~ models[2],
      cond == "UN" | cond == "US" ~ models[3],
      cond == "RS" | cond == "RN" ~ models[4]
  ),
  cover_story = ifelse(grepl("S",cond),"Cover Story", "No Cover Story")
)

colour_scale <- c("seagreen", "red", "orange","lightblue")

odds_ratio <- all_plotting %>%
  filter(model != "Random") %>%
  mutate(odds_ratio = posterior/random_prob)

# function that creates box plots from model predictions on empirical data
boxPlotLearner = function(odds_ratio, title, subtitle, or = TRUE){
  d_plot <- odds_ratio %>%
    mutate(model = factor(model, levels = models),
           full_cond = factor(full_cond, levels = models))

  plot <- d_plot %>%
    ggplot(aes(x = model, y = odds_ratio, fill = model)) +
    geom_jitter(aes(colour = model), alpha = .2) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1, colour = "black") +
    #geom_bar(position="dodge", stat="identity")+
    scale_fill_manual(values = colour_scale) +
    scale_colour_manual(values = colour_scale) +
    scale_x_discrete(labels = models) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Learner Model",
      y = "Odds Ratio (Compared to Random Sampling Model)"
    ) +
    ylim(0,25)+
    facet_grid(~full_cond ~ experiment + cover_story) +  # facet_grid( ~ full_cond)+#, labeller = labeller(provider_cond = cond_labels)) +
    theme_bw() +
    theme(
      legend.position =  "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(size = 22),
      text = element_text(size = 18),
      axis.text.y = element_text(size = 14),
      line = element_blank(),
      strip.background = element_rect(fill = "white", colour = "black")
    )
  
  plot
}

file <- paste0("experiment-comparison/posterior-boxplot-all-exps-b",plot_block,"-c",plot_clue)

if(filter_cover_check){
  file <- paste0(file,"-filter-cover-", filter_cover_check)
}


boxPlotLearner(odds_ratio, title = "Performance of different learner models in each condition", subtitle = NULL)
ggsave(here(paste0(file,".png")),  width = 15, height = 8)
