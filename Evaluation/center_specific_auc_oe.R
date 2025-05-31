library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(pROC)
library(ggplot2)
library(ggbeeswarm)  # for better spread than jitter

#import center info data
center_info <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/center_info.csv")

### only for 5_5_1000
## my metrics dataframe has auc and cases(pos), expect_prob group by center
df_center <- read_csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost/results_5_5_1000_TRUE.csv") %>%
  mutate(model_type = "Type of center") %>%
  select(outcome1, center, oncocenter, starts_with("probabilities_"))

df_nocenter <- read_csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost/results_5_5_1000_FALSE.csv") %>%
  mutate(model_type = "No higher-order variable") %>%
  select(outcome1, center, starts_with("probabilities_"))

df_full_prev <- read_csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost/results_5_5_1000_FALSE_prev_large.csv") %>%
  mutate(model_type = "Prevalence full training set") %>%
  select(outcome1, center, starts_with("probabilities_"))

df_small_prev <- read_csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost/results_5_5_1000_FALSE_prev_small..csv") %>%
  mutate(model_type = "Prevalence small training set") %>%
  select(outcome1, center, starts_with("probabilities_"))

# for random forest change x1 -> 1
# df_center$outcome1 <- ifelse(df_center$outcome1 == "X1", 1, 0) # for random forest otherwise remove 
# df_nocenter$outcome1 <- ifelse(df_nocenter$outcome1 == "X1", 1, 0) # for random forest otherwise remove 
# df_full_prev$outcome1 <- ifelse(df_full_prev$outcome1 == "X1", 1, 0) # for random forest otherwise remove 
# df_small_prev$outcome1 <- ifelse(df_small_prev$outcome1 == "X1", 1, 0) # for random forest otherwise remove 

# calculate the auc per center and per iteration 
calculate_centerwise_auc <- function(df, outcome_col = "outcome1", center_col = "center", prob_prefix = "probabilities_") {
  
  # Identify probability columns by prefix
  prob_cols <- grep(paste0("^", prob_prefix), names(df), value = TRUE)
  
  # Nest data by center
  center_nested <- df %>%
    group_by(.data[[center_col]]) %>%
    nest()
  
  # Function to compute AUCs for a single center's data
  calc_center_auc <- function(data) {
    outcome <- data[[outcome_col]]
    aucs <- map_dbl(prob_cols, function(prob_col) {
      tryCatch({
        roc_obj <- roc(outcome, data[[prob_col]], quiet = TRUE)
        auc(roc_obj)
      }, error = function(e) NA_real_)
    })
    c(n = nrow(data),
      mean_outcome = mean(outcome),
      setNames(aucs, paste0("auc_", gsub(prob_prefix, "", prob_cols))))
  }
  
  # Apply per center and unnest
  result_df <- center_nested %>%
    mutate(auc_info = map(data, calc_center_auc)) %>%
    select(center = .data[[center_col]], auc_info) %>%
    unnest_wider(auc_info)
  
  return(result_df)
}

# apply to all datasets
auc_center <- calculate_centerwise_auc(df_center) %>%
  mutate(model_type = "Center Type")

auc_nocenter <- calculate_centerwise_auc(df_nocenter) %>%
  mutate(model_type = "No higher-order variable")

auc_full_prev <- calculate_centerwise_auc(df_full_prev) %>%
  mutate(model_type = "True Prevalence")

auc_small_prev <- calculate_centerwise_auc(df_small_prev) %>%
  mutate(model_type = "Estimated Prevalence")

# Combine all into one dataframe
all_auc <- bind_rows(auc_center, auc_nocenter, auc_small_prev, auc_full_prev)


# add type of center 
all_auc <- left_join(all_auc, center_info, by = 'center')

# AUC columns to long format
long_auc <- all_auc %>%
  pivot_longer(
    cols = starts_with("auc_"),
    names_to = "iteration",
    values_to = "AUC"
  )

# mean and 95% CI per center and model_type
summary_df_auc <- long_auc %>%
  group_by(center, model_type) %>%
  summarise(
    mean_auc = mean(AUC, na.rm = TRUE),
    sd_auc = sd(AUC, na.rm = TRUE),
    n = n(),
    se = sd_auc / sqrt(n),
    lower = mean_auc - 1.96 * se,
    upper = mean_auc + 1.96 * se,
    .groups = "drop"
  ) %>%
  left_join(center_info, by = "center")


######### oe-ratio ##########
calculate_centerwise_oe <- function(df, outcome_col = "outcome1", center_col = "center", prob_prefix = "probabilities_") {
  
  # Identify probability columns by prefix
  prob_cols <- grep(paste0("^", prob_prefix), names(df), value = TRUE)
  
  # Nest data by center
  center_nested <- df %>%
    group_by(.data[[center_col]]) %>%
    nest()
  
  # Function to compute O/E ratios for a single center's data
  calc_center_oe <- function(data) {
    outcome <- data[[outcome_col]]
    oes <- map_dbl(prob_cols, function(prob_col) {
      expected <- sum(data[[prob_col]], na.rm = TRUE)
      observed <- sum(outcome, na.rm = TRUE)
      if (expected == 0) return(NA_real_)
      observed / expected
    })
    c(n = nrow(data),
      mean_outcome = mean(outcome),
      setNames(oes, paste0("oe_", gsub(prob_prefix, "", prob_cols))))
  }
  
  # Apply per center and unnest
  result_df <- center_nested %>%
    mutate(oe_info = map(data, calc_center_oe)) %>%
    select(center = .data[[center_col]], oe_info) %>%
    unnest_wider(oe_info)
  
  return(result_df)
}


# Apply to each scenario
oe_center <- calculate_centerwise_oe(df_center) %>%
  mutate(model_type = "Type of center")
oe_nocenter <- calculate_centerwise_oe(df_nocenter) %>%
  mutate(model_type = "No higher-order variable")
oe_full_prev <- calculate_centerwise_oe(df_full_prev) %>% 
  mutate(model_type = "Prevalence full training set")
oe_small_prev <- calculate_centerwise_oe(df_small_prev) %>%
  mutate(model_type = "Prevalence small training set")

# Combine into one long dataset
all_oe <- bind_rows(oe_center, oe_nocenter, oe_full_prev, oe_small_prev)

# add type of center 
all_oe <- left_join(all_oe, center_info, by = 'center')

# AUC columns to long format
long_oe <- all_oe %>%
  pivot_longer(
    cols = starts_with("oe_"),
    names_to = "iteration",
    values_to = "oe_ratio"
  )

summary_df_oe <- long_oe %>%
  group_by(center, model_type) %>%
  summarise(
    mean_oe = mean(oe_ratio, na.rm = TRUE),
    sd_oe = sd(oe_ratio, na.rm = TRUE),
    n = n(),
    se = sd_oe / sqrt(n),
    lower = mean_oe - 1.96 * se,
    upper = mean_oe + 1.96 * se,
    .groups = "drop"
  ) %>%
  left_join(center_info, by = "center")

# plot legend in correct order
summary_df_auc$model_type <- factor(summary_df_auc$model_type, levels = c(
  "No higher-order variable",
  "Center Type",
  "True Prevalence",
  "Estimated Prevalence"
))



ggplot(summary_df_auc, aes(x = factor(center, levels = sort(unique(center))), y = mean_auc, color = model_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  scale_color_manual(
    values = c(
      "No higher-order variable" = "lightsteelblue1",    
      "Center Type" = "thistle",       
      "True Prevalence" = "palegreen3", 
      "Estimated Prevalence" = "wheat1"
    )) +
  coord_flip() +
  ylim(0.5, 1.2) +  # set x-axis (flipped) range from 0 to 2
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    x = "Center",
    y = "Mean AUC (95% CI)",
    color = "Higher-Order Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),           
    plot.margin = margin(5, 5, 5, 5),               
    panel.spacing = unit(1, "lines"),            
    aspect.ratio = 1.2,                            
    # legend.position = "none"
  )


ggplot(summary_df_oe, aes(x = factor(center, levels = sort(unique(center))), y = mean_oe, color = model_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  scale_color_manual(
    values = c(
      "No higher-order variable" = "lightsteelblue1",    
      "Type of center" = "thistle",       
      "Prevalence full training set" = "palegreen3", 
      "Prevalence small training set" = "wheat1"
    )) +
  coord_flip() +
  ylim(0, 2) +  # set x-axis (flipped) range from 0 to 2
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    x = "Center",
    y = "Mean O:E Ratio (95% CI)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),         
    plot.margin = margin(5, 5, 5, 5),              
    panel.spacing = unit(1, "lines"),            
    aspect.ratio = 1.2,                             
    legend.position = "none"
  )


###### net benefit #####

all_net_benefits <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/net_benefits/net_benefit_xgboost.csv")

# pick only the right model's net benefit 
all_nb <- all_net_benefits[grepl("^results_5_5_1000", all_net_benefits$Dataset), ]

# Summarize NB over models (per Dataset and center)
summary_df_nb <- all_nb %>%
  group_by(Dataset, center) %>%
  summarise(
    mean_nb = mean(NB, na.rm = TRUE),
    sd_nb = sd(NB, na.rm = TRUE),
    n = n(),
    se = sd_nb / sqrt(n),
    lower = mean_nb - 1.96 * se,
    upper = mean_nb + 1.96 * se,
    .groups = "drop"
  )

load("/Users/ebbablomdahl/Thesis/Thesis/datasets/entire_train_set_prevalence_10000_per_center.RData")

# Set decision threshold
threshold <- 0.1

treat_all_nb <- prevalence_df %>%
  mutate(
    NB_treat_all = prevalence - (1 - prevalence) * threshold / (1 - threshold)
  ) %>%
  select(center, NB_treat_all)

summary_df_nb <- summary_df_nb %>%
  left_join(treat_all_nb, by = "center")


summary_df_nb <- summary_df_nb %>%
  left_join(center_info, by = "center")

# Add a column for the treat_all indicator (optional, for clarity)
summary_df_nb <- summary_df_nb %>%
  mutate(treat_all_label = "Treat All")

ggplot(summary_df_nb, aes(x = factor(center, levels = sort(unique(center))), y = mean_nb, color = Dataset)) +
  # Add reference points for each center's treat_all value
  geom_point(
    data = summary_df_nb,
    aes(x = factor(center, levels = sort(unique(center))), y = NB_treat_all, shape = "Treat All"),
    inherit.aes = FALSE,
    shape = 18,
    size = 3,
    color = "#D75455"
  ) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  scale_color_manual(
    values = c(
      "results_5_5_1000_FALSE" = "lightsteelblue1",    
      "results_5_5_1000_TRUE" = "thistle",       
      "results_5_5_1000_FALSE_prev_large" = "palegreen3", 
      "results_5_5_1000_FALSE_prev_small" = "wheat1",
      "Treat All" = "#D75455"
    )) +
  coord_flip() +
  ylim(-0.1, 0.9) +  # set x-axis (flipped) range from 0 to 2
  # geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Center",
    y = "Mean Net Benefit Ratio (95% CI)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),       
    plot.margin = margin(5, 5, 5, 5),             
    panel.spacing = unit(1, "lines"),            
    aspect.ratio = 1.2,                            
    #legend.position = "none"
  )

