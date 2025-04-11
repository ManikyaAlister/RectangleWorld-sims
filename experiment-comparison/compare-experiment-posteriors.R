rm(list = ls())
library(here)
library(effsize)
library(BayesFactor)
source(here("functions/plottingFunctions.R"))
load(here("experiment-1/data/derived/all_conditions.Rdata"))


experiments = c(1,2,3)
blocks = 8
clues = 2:4 # discard the first clue as it's not very informative
target_blocks <- c(2,8)



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
    conditions == "HN" | conditions == "HS" ~ recursion_conds["helpful"],
    conditions == "RN" | conditions == "RS" ~ recursion_conds["random"],
    conditions == "MN" |
      conditions == "MS" ~ recursion_conds["misleading"],
    conditions == "UN" |
      conditions == "US" ~ recursion_conds["uninformative"]
  ),
  provider = case_when(
    conditions == "HS" | conditions == "HN" ~ "helpful",
    conditions == "MS" | conditions == "MN" ~ "misleading",
    conditions == "UN" | conditions == "US" ~ "uninformative",
    conditions == "RS" | conditions == "RN" ~ "random"
  )
)

c <- clues
b <- blocks



# filter relevant conditions
all_conditions_tmp <- all_conditions %>%
  filter(blocks == b & clues %in% c, conditions %in% c("HS", "MS", "US"))

posterior_data <- NULL


for (exp in experiments){
  
for (i in 1:length(all_conditions_tmp[, 1])) {
  condition <- all_conditions_tmp[i, "conditions"]
  model_alpha <- all_conditions_tmp[i, "recovery_alpha"]
  recursion <- all_conditions_tmp[i, "recursion"]
  provider <- all_conditions_tmp[i, "provider"]
  b <- all_conditions_tmp[i, "blocks"]
  
  if (b %in% target_blocks){
    # load posteriors 
    if (recursion) {
      # load posteriors for participant data
      load(here(
        paste0(
          "experiment-",
          exp,
          "/modelling/04_output/tb",b,"-all-alpha-posteriors-recursive.Rdata"
        )
      ))
      
    } else {
      # load posteriors for participant data
      load(here(
        paste0(
          "experiment-",
          exp,
          "/modelling/04_output/tb",b,"-all-alpha-posteriors.Rdata"
        )
      ))
    }
  }  else {
    if (recursion) {
      # load posteriors for participant data
      load(here(paste0("experiment-",exp,"/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,"-recursive.Rdata")))
      
    } else {
      # load posteriors for participant data
      load(here(paste0("experiment-",exp,"/modelling/04_output/b",b,"-all-alpha-posteriors-",provider,".Rdata")))
    }
  }
  
  
  d_iteration <- all_alpha_posteriors %>%
    filter(cond == condition, alpha == model_alpha) %>%
    mutate(block = b)
  
  d_iteration$experiment <- as.character(exp)
  
  posterior_data <- rbind(posterior_data, d_iteration)
  
}
}


# to be a t test, need 1 data point for each participant. 
posterior_data_by_subj <- posterior_data %>%
  group_by(pid, experiment, cond) %>%
  summarise(posterior = median(posterior))

# best if data is normally distributed. 
posterior_data_by_subj %>%
  mutate(experiment = as.character(experiment)) %>%
  group_by(experiment, cond) %>%
  summarise(stat= median(posterior), se = sd(posterior)/sqrt(n())) %>%
  ggplot(aes(x = experiment, y = stat, fill = experiment)) +
  geom_col() +
  #geom_jitter(data = posterior_data, aes(y = posterior), alpha = 0.05)+
  geom_errorbar(aes(ymin = stat - se, ymax = stat + se), width = 0.2) +
  labs(y = "Posterior", subtitle = "Median posterior of participant responses according to the learner model of the condition the participant was in")+
  facet_wrap(~cond, nrow = 1, scales = "free")

conds <- unique(all_conditions_tmp$conditions)

experiment_comparisons <- rbind(c(1,2), c(2,3))

experimentComparisonStats = function(data, conds, experiment_comparisons, shapiro = FALSE, normal_plot = TRUE, rm_random = TRUE){
  
  comparison_stats <- NULL
  
  random_conds <- c("RS", "RN")
  
  if(rm_random){
  conds <- conds[!conds %in% random_conds]  
  }
  
  for(condition in conds) {
    for (j in 1:nrow(experiment_comparisons)) {
      compare_experiments <- experiment_comparisons[j,]  
      
      cond_data <- data %>%
        filter(cond == condition, experiment %in% compare_experiments)
      
      # outcome variable normality
      if(normal_plot){
        normality <- cond_data %>%
          ggplot(aes(x = posterior)) +
          geom_histogram()+
          facet_grid(~experiment)+
          labs(title = paste0("Histogram of posterior: ",condition))
        print(normality)
      }
      if(shapiro){
        shapiro_results <- posterior_data_by_subj %>%
          group_by(experiment) %>%
          summarize(shapiro_test_statistic = shapiro.test(posterior)$statistic,
                    p_value = round(shapiro.test(posterior)$p.value),4)
        print(shapiro_results)
      }
      bf <- BayesFactor::ttestBF(data = cond_data, formula = posterior ~ experiment)
      bayes_factor <- unname(as.vector(bf))
      wilcoxin <- wilcox.test(posterior ~ experiment, data = cond_data) 
      W <- wilcoxin$statistic
      p <- wilcoxin$p.value
      cd <- cliff.delta(cond_data$posterior, cond_data$experiment)
      cliff_delta <- cd$estimate
      cliff_delta_lower <- cd$conf.int[1]
      cliff_delta_upper <- cd$conf.int[2]
      cliff_delta_magnitude <- as.character(cd$magnitude)
      exp_comparison <- paste0(compare_experiments[1], " v ", compare_experiments[2])
      row_data <- cbind(W,p,cliff_delta, cliff_delta_lower, cliff_delta_upper, cliff_delta_magnitude, condition, exp_comparison)
      comparison_stats <- rbind(comparison_stats, row_data)
    }
  }
  comparison_stats <- as.data.frame(comparison_stats)
  comparison_stats$p_adjust <- round(p.adjust(comparison_stats$p, method = "bonferroni"),4)
  comparison_stats
}
# 
 comparison_stats <- experimentComparisonStats(posterior_data_by_subj, conds, experiment_comparisons, normal_plot = FALSE)
# 
# experiment_comparisons <- rbind(c(1,2), c(2,3)) # uncomment once experiment 3 is run
# 
# comparison_stats_e3 <- experimentComparisonStats(posterior_data_by_subj, conds, experiment_comparisons, normal_plot = FALSE)


# interpretation of cliff's delta: J. Romano, Appropriate statistics for ordinal level data: Should we really be using t-test and cohen's d for evaluating group differences on the NSSE and other surveys?
