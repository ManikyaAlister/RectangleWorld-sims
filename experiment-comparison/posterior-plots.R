rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
source(here("functions/plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.Rdata"))

sum_all <- NULL
recovery_all <- NULL
block_plot <- 8
simple <- FALSE # if set to simple, only three alpha values alligning with three provider types

if (simple){
  model_alphas <- c(-1,0,1)
} else {
  model_alphas <- c(-5,-2,-1,-0.5,-0.1,0, 0.1,0.5,1,2,5)
}


for (exp in 1:3) {
  #c = 4 # clue
  
  # function for wrangling plot data
  getPosteriorPlotData = function(posteriors, model_alphas, simple_model = TRUE) {
    plot_data <- posteriors %>%
      mutate(alpha = as.factor(alpha)) %>%
      filter(alpha %in% model_alphas) %>%
      group_by(alpha) %>%
      summarise(
        mean = mean(posterior),
        median = median(posterior),
        sum = sum(posterior),
        se = sd(posterior) / sqrt(n())
      )
    
    if (!simple){
      plot_data <- plot_data %>%
      #  normalize so that everything is on the same scale because otherwise it's hard to read with full alphas
        mutate(
          mean = mean / sum(mean),
          median = median / sum(median),
          sum = sum / sum(sum),
          se = se / sum(mean)
        )
    }
    
    
    plot_data
  }
  
  
  
  # Define alphas for each condition for recovery comparison
  alphas = c(
    helpful = 1,
    random = 0,
    misleading = -1,
    uninformative = -1
  )
  
  # Define whether each condition is recursive
  recursion_conds = c(
    helpful = FALSE,
    random = FALSE,
    misleading = FALSE,
    uninformative = TRUE
  )
  
  # Define recovery alpha and recursion for each condition
  all_conditions <- all_conditions %>% mutate(
    recovery_alpha = case_when(
      conditions == "HN" | conditions == "HS" ~ alphas["helpful"],
      conditions == "RN" | conditions == "RS" ~ alphas["random"],
      conditions == "MN" | conditions == "MS" ~ alphas["misleading"],
      conditions == "UN" |
        conditions == "US" ~ alphas["uninformative"]
    ),
    recursion = case_when(
      conditions == "HN" |
        conditions == "HS" ~ recursion_conds["helpful"],
      conditions == "RN" |
        conditions == "RS" ~ recursion_conds["random"],
      conditions == "MN" |
        conditions == "MS" ~ recursion_conds["misleading"],
      conditions == "UN" |
        conditions == "US" ~ recursion_conds["uninformative"]
    ),
    provider = case_when(
      conditions == "HS" | conditions == "HN" ~ "Helpful",
      conditions == "MS" | conditions == "MN" ~ "Misleading\nNaive",
      conditions == "UN" | conditions == "US" ~ "Misleading\nAware",
      conditions == "RS" | conditions == "RN" ~ "Random"
    ),
    cover_story = ifelse(grepl("S", conditions), "Cover Story", "No Cover Story")
  )
  
  if (exp == 3) {
    # remove no cover story condition since that was not in E3
    all_conditions <- all_conditions %>%
      filter(conditions %in% c("HS", "RS", "US", "MS"))
  }
  
  
  # re order for plotting
  all_conditions <- all_conditions[order(as.character(all_conditions$conditions)), ]
  
  
  
  target_blocks <- c(2, 8)
  clues <- 1:4
  providers <- c("helpful", "random", "misleading", "uninformative")
  
  for (b in block_plot) {
    for (c in clues) {
      print(paste0("block ", b))
      print(paste0("clue ", c))
      # All conditions have the same provider in target blocks
      if (b %in% target_blocks) {
        provider <- FALSE
      }
      
      # filter relevant conditions
      all_conditions_tmp <- all_conditions %>%
        filter(blocks == b & clues == c)
      
      plot_list = NULL
      for (i in 1:length(all_conditions_tmp[, 1])) {
        condition <- all_conditions_tmp[i, "conditions"]
        alpha <- all_conditions_tmp[i, "recovery_alpha"]
        recursion <- all_conditions_tmp[i, "recursion"]
        provider <- all_conditions_tmp[i, "provider"]
        cover_story <- all_conditions_tmp[i, "cover_story"]
        
        if (b %in% target_blocks) {
          # load posteriors
          if (recursion) {
            # load posteriors for participant data
            load(here(
              paste0(
                "experiment-",
                exp,
                "/modelling/04_output/tb",
                b,
                "-all-alpha-posteriors-recursive.Rdata"
              )
            ))
            # load posteriors for recovery
            load(here(
              paste0(
                "recovery/data/a",
                alpha,
                "_n100_c",
                c,
                "_pr-flat_recursion.RData"
              )
            ))
            
          } else {
            # load posteriors for participant data
            load(here(
              paste0(
                "experiment-",
                exp,
                "/modelling/04_output/tb",
                b,
                "-all-alpha-posteriors.Rdata"
              )
            ))
            # load posteriors for recovery
            load(here(
              paste0(
                "recovery/data/a",
                alpha,
                "_n100_c",
                c,
                "_pr-flat.RData"
              )
            ))
          }
        }  else {
          if (recursion) {
            # load posteriors for participant data
            load(here(
              paste0(
                "experiment-",
                exp,
                "/modelling/04_output/b",
                b,
                "-all-alpha-posteriors-",
                provider,
                "-recursive.Rdata"
              )
            ))
            # load posteriors for recovery
            load(here(
              paste0(
                "recovery/data/a",
                alpha,
                "_n100_c",
                c,
                "_pr-flat_b_",
                b,
                "_",
                provider,
                "_recursion.RData"
              )
            ))
            
          } else {
            # load posteriors for participant data
            load(here(
              paste0(
                "experiment-",
                exp,
                "/modelling/04_output/b",
                b,
                "-all-alpha-posteriors-",
                provider,
                ".Rdata"
              )
            ))
            # load posteriors for recovery
            load(here(
              paste0(
                "recovery/data/a",
                alpha,
                "_n100_c",
                c,
                "_pr-flat_b_",
                b,
                "_",
                provider,
                "_.RData"
              )
            ))
          }
        }
        
        # wrangle recovery data for plotting
        recovery_plotting <- getPosteriorPlotData(posteriors, model_alphas, simple_model = simple) %>%
          mutate(
            condition = condition,
            provider = provider,
            clue = c,
            block = b,
            experiment = paste0("Exp. ", exp),
            cover_story = cover_story
          )
        
        recovery_all <- rbind(recovery_all, recovery_plotting)
        
        # wrangle participant data for plotting
        all_data <- all_alpha_posteriors %>%
          filter(cond == condition, clue == c) %>%
          mutate(alpha = as.factor(alpha))
        
        sum <- getPosteriorPlotData(all_data, model_alphas) %>%
          mutate(
            condition = condition,
            provider = provider,
            clue = c,
            block = b,
            experiment = paste0("Exp. ", exp),
            cover_story = cover_story
          )
        
        sum_all <- rbind(sum_all, sum)
        
        plot <-
          plotPosteriors(
            p_data = sum,
            statistic = "median",
            recovery_data = recovery_plotting,
            subtitle = ""
          )
        
        if (b %in% target_blocks) {
          ggsave(
            here(
              paste0(
                "experiment-",
                exp,
                "/modelling/05_plots/posteriors/posteriors-",
                condition,
                "-c-",
                c,
                "-b-",
                b,
                ".png"
              )
            ),
            width = 7,
            height = 5,
            plot = plot
          )
        } else {
          ggsave(
            here(
              paste0(
                "experiment-",
                exp,
                "/modelling/05_plots/posteriors/posteriors-",
                condition,
                "-c-",
                c,
                "-b-",
                b,
                "-",
                provider,
                ".png"
              )
            ),
            width = 7,
            height = 5,
            plot = plot
          )
        }
        # save individual plots
        
        # save to list so they can be plotted together
        plot_list[[i]] <- plot
      }
      # re-order the list so plots of the same condition are next to each other
      #plot_list <- plot_list[c(2:length(plot_list), 1)]
      
      #labels = c() # to fill in with condition names
      combined_plot = ggpubr::ggarrange(plotlist =  plot_list,
                                        ncol = 2,
                                        nrow = 4)
      combined_plot
      
      if (b %in% target_blocks) {
        ggsave(
          here(
            paste0(
              "experiment-",
              exp,
              "/modelling/05_plots/posteriors/posteriors-all-conditions-c-",
              c,
              "-b-",
              b,
              ".png"
            )
          ),
          width = 8,
          height = 10,
          plot = combined_plot
        )
      } else {
        ggsave(
          here(
            paste0(
              "experiment-",
              exp,
              "/modelling/05_plots/posteriors/posteriors-all-conditions-c-",
              c,
              "-b-",
              b,
              ".png"
            )
          ),
          width = 8,
          height = 10,
          plot = combined_plot
        )
      }
      
    }
  }
  
}



clue_plot <- 4
recovery_all_plotting <- recovery_all %>%
  filter(clue == clue_plot, block == block_plot)

  

sum_all_plotting <- sum_all %>%
  filter(clue == clue_plot, block == block_plot)



plotPosteriorsCombined = function(p_data, statistic, man_check = FALSE, recovery_data, title = "", subtitle = "", simple_plot = simple) {
  
  # Define consistent factor levels
  provider_levels <- c("Helpful", "Misleading\nNaive", "Misleading\nAware", "Random")
  
  # Apply to both data sets
  p_data <- p_data %>%
    mutate(
      alpha = as.factor(alpha),
      provider = factor(provider, levels = provider_levels)
    )
  
  recovery_data <- recovery_data %>%
    mutate(
      alpha = as.factor(alpha),
      provider = factor(provider, levels = provider_levels)
    )
  
  # custom colour scaling
  colfunc <- colorRampPalette(c("red", "skyblue3","darkgreen"))
  

  if(simple_plot){
    title = "Performance of different learner models in each condition"
    x_title = "Learner Model"
  } else {
    title =bquote("Inferred " * alpha * " for participant data in each condition")
    x_title = bquote(alpha * ": < 0 (red), misleading assumption; > 0 (green) helpful assumption")
  }
  
  plot <- p_data %>% 
    ggplot() +
    geom_col(aes(x = alpha, y = eval(parse(text = statistic)), fill = alpha)) +
    geom_errorbar(
      aes(
        x = alpha,
        ymin = eval(parse(text = statistic)) - se,
        ymax = eval(parse(text = statistic)) + se
      ),
      width = 0.2   # controls the width of the little crossbars
    )+
    theme_classic() +
    scale_fill_manual(values = setNames(colfunc(length(unique(p_data$alpha))), unique(p_data$alpha)))+
  #scale_fill_brewer(palette = "RdBlGn") +
    labs(
      y = "Median Posterior Probability",
      x = x_title,
      title = title,
      subtitle = subtitle
    ) +
    theme(
      axis.ticks.y = element_blank(),
      text = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(angle = 45, vjust = .5),
      axis.title.x = element_text(size = 22),
      legend.position = "none"
    ) +
    facet_grid(provider ~ experiment + cover_story)

  if(simple){
    plot <- plot +
      scale_x_discrete(
        breaks = c(-1, 0, 1),
        labels = c("Misleading*", "Random", "Helpful")
      )
  } else{
    plot <- plot +
      geom_line(data = recovery_data, aes(x = alpha, y = eval(parse(text = statistic))), group = 1)
  }
  
  plot
}



plot <- plotPosteriorsCombined(
  sum_all_plotting,
  "median",
  recovery_data = recovery_all_plotting,
  title = title,
  subtitle = NULL
)

plot

if(simple){
  file <- paste0("experiment-comparison/posterior-all-exps-b",block_plot,"-c",clue_plot,"-simple.png")
}else {
  file <- paste0("experiment-comparison/posterior-all-exps-b",block_plot,"-c",clue_plot,".png")
  
}



ggsave(filename = here(file), width = 15, height = 8)


