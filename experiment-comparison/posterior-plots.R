rm(list = ls())
library(here)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
source(here("functions/plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.R"))

sum_all <- NULL
recovery_all <- NULL
block_plot <- 2


for (exp in 1:3) {
  #c = 4 # clue
  
  # function for wrangling plot data
  getPosteriorPlotData = function(posteriors) {
    plot_data <- posteriors %>%
      mutate(alpha = as.factor(alpha)) %>%
      group_by(alpha) %>%
      summarise(
        mean = mean(posterior),
        median = median(posterior),
        sum = sum(posterior)
      ) %>%
      # normalize so that everything is on the same scale
      mutate(
        mean = mean / sum(mean),
        median = median / sum(median),
        sum = sum / sum(sum)
      )
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
        recovery_plotting <- getPosteriorPlotData(posteriors) %>%
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
        
        sum <- getPosteriorPlotData(all_data) %>%
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



plotPosteriorsCombined = function(p_data, statistic, man_check = FALSE, recovery_data, title = "", subtitle = "") {
  
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

  plot <- p_data %>% 
    ggplot() +
    geom_col(aes(x = alpha, y = eval(parse(text = statistic)), fill = alpha)) +
    geom_line(data = recovery_data, aes(x = alpha, y = eval(parse(text = statistic))), group = 1) +
    theme_classic() +
    scale_fill_brewer(palette = "RdYlGn") +
    labs(
      y = "Median Posterior Probability of Participant Inferences",
      x = bquote(alpha * ": < 0 (red), misleading assumption; > 0 (green) helpful assumption"),
      title = title,
      subtitle = subtitle
    ) +
    theme(
      axis.ticks.y = element_blank(),
      text = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(angle = 60, vjust = .5),
      axis.title.x = element_text(size = 22),
      legend.position = "none"
    ) +
    facet_grid(provider ~ experiment + cover_story)

  plot
}


plotPosteriorsCombined(
  sum_all_plotting,
  "median",
  recovery_data = recovery_all_plotting,
  title = bquote("Inferred " * alpha * " for participant data in each condition"),
  subtitle = NULL
)

ggsave(filename = here(paste0("experiment-comparison/posterior-all-exps-b",block_plot,"-c",clue_plot,".png")), width = 15, height = 8)


