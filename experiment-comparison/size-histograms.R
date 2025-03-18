rm(list = ls())
# define experiments to be compared
experiments <- c(1:3,3)
filtered <- c(FALSE, FALSE, FALSE, TRUE)
all_experiment_data <- NULL 

for (i in 1:length(experiments)) {
  filtered_i <- filtered[i]
  if (filtered_i){
    exp <- experiments[i]
    experiment_name <- paste0("Exp. ",exp, " Filtered" )
    load(here(paste0("experiment-",exp,"/data/derived/d_size_histograms_filtered.Rdata")))
  } else {
    exp <- experiments[i]
    experiment_name <- paste0("Exp. ",exp )
    load(here(paste0("experiment-",exp,"/data/derived/d_size_histograms.Rdata")))
    
  }
  if (i == 3){
    d_all_sizes <- d_all_sizes %>%
      filter(prior_type == "flat") %>%
      select(-prior_type)
  }
  
  d_iteration <- d_all_sizes %>%
    mutate(experiment = experiment_name)

  all_experiment_data <- rbind(all_experiment_data, d_iteration)    
}

  cond_labels <- c(
    "helpful" = "Helpful",
    "misleading" = "Misleading\nNaive",
    "uninformative" = "Misleading\nAware",
    "random" = "Random"
  )  

all_experiment_data %>%
  mutate(size = factor(size),
         learn_cond = factor(case_when(
           cond == "HS" | cond == "HN" ~ "helpful",
           cond == "MS" | cond == "MN" ~ "misleading", 
           cond == "UN" | cond == "US" ~ "uninformative",
           cond == "RS" | cond == "RN" ~ "random"
         ), levels = names(cond_labels)),
         cover_cond = case_when(
           str_detect(cond, "N") ~ "No Cover Story",
           str_detect(cond, "S") ~ "Cover Story"
         )) %>%
     ggplot(aes(x = size)) +
    geom_col(aes(y = Percent, fill = cond)) +
    geom_line(aes(y = prob, group = cond),linewidth = 0.4, colour = "grey28")+
    scale_fill_manual(values = c("HS" = "darkgreen", "HN" = "darkgreen", "RS" = "lightblue", "RN" = "lightblue", "MS" = "darkred", "MN" = "darkred", "UN" = "orange", "US" = "orange"))+
    theme_bw()+
    labs(x = "Rectangle Size (Smallest to Largest)")+
    theme(axis.text.x=element_blank(),
          #axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          line = element_blank(),
          strip.background = element_rect(fill= "white"),
          #text = element_text(size = 25),
          #axis.text = element_text(size = 22),
          #strip.text = element_text(margin = margin(0,0,0,0, "cm"), size = 5),
          legend.position = "none")+
    facet_grid(learn_cond ~ experiment+cover_cond, labeller = labeller(learn_cond = cond_labels))

ggsave(here("experiment-comparison/size-hist.png"), width = 7.5, height = 5)
  