rm(list = ls())
library(here)
library(tidyverse)
library(effsize)

experiments <- 1:3
target_blocks <- c(2,8)
all_accuracy <- NULL 

# load accuracy data 
for (i in experiments){
  load(here(paste0("experiment-",i,"/data/derived/accuracy.Rdata")))
  d$experiment <- i
  all_accuracy <- rbind(all_accuracy, d)
}

# do not include target blocks, since those points are not actually chosen by the model. 
all_accuracy <- as.data.frame(all_accuracy) %>%
  mutate(experiment = as.character(experiment)) %>%
  filter(!block %in% target_blocks)

sum_accuracy <- all_accuracy %>%
  group_by(pid, experiment, cond) %>%
  summarise(accuracy = mean(accuracy)) 

sum_accuracy %>% 
  group_by(cond, experiment) %>%
  summarise(acc = mean(accuracy), se = sd(accuracy)/sqrt(n()))%>%
  ggplot(aes(x = experiment, y = acc, fill = experiment)) +
  geom_col() +
  geom_errorbar(aes(ymin = acc - se, ymax = acc + se), width = 0.2) +
  facet_wrap(~cond, nrow = 1)

conds <- c(
  "HS",
  "RS",
  "MS",
  "US"
)

experiment_comparisons <- rbind(c(1,2), c(2,3))

experimentComparisonStats = function(data, conds, experiment_comparisons, shapiro = FALSE, normal_plot = TRUE){
  
  comparison_stats <- NULL
  
  for(condition in conds) {
    for (j in 1:nrow(experiment_comparisons)) {
      compare_experiments <- experiment_comparisons[j,]  
      
      cond_data <- data %>%
        filter(cond == condition, experiment %in% compare_experiments)
      
      # outcome variable normality
      if(normal_plot){
        normality <- cond_data %>%
          ggplot(aes(x = accuracy)) +
          geom_histogram()+
          facet_grid(~experiment)+
          labs(title = paste0("Histogram of accuracy: ",condition))
        print(normality)
      }
      if(shapiro){
        print(condition)
        shapiro_results <- cond_data %>%
          group_by(experiment) %>%
          summarize(shapiro_test_statistic = shapiro.test(accuracy)$statistic,
                    p_value = round(shapiro.test(accuracy)$p.value,4))
        print(shapiro_results)
      }
      bf <- BayesFactor::ttestBF(data = cond_data, formula = accuracy ~ experiment)
      bayes_factor <- unname(as.vector(bf))
      wilcoxin <- wilcox.test(accuracy ~ experiment, data = cond_data) 
      W <- wilcoxin$statistic
      p <- wilcoxin$p.value
      cd <- cliff.delta(cond_data$accuracy, cond_data$experiment)
      cliff_delta <- cd$estimate
      cliff_delta_lower <- cd$conf.int[1]
      cliff_delta_upper <- cd$conf.int[2]
      cliff_delta_magnitude <- as.character(cd$magnitude)
      exp_comparison <- paste0(compare_experiments[1], " v ", compare_experiments[2])
      row_data <- cbind(bayes_factor, W,p,cliff_delta, cliff_delta_lower, cliff_delta_upper, cliff_delta_magnitude, condition, exp_comparison)
      comparison_stats <- rbind(comparison_stats, row_data)
    }
  }
  comparison_stats <- as.data.frame(comparison_stats)
  comparison_stats$p_adjust <- round(p.adjust(comparison_stats$p, method = "bonferroni"),4)
  comparison_stats
}

accuracy_stats <- experimentComparisonStats(sum_accuracy, conds, experiment_comparisons, normal_plot = TRUE, shapiro = TRUE)
