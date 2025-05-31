#### script to merge all the tau data and then plot it ####
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

### read the taus ###

auc_tau <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2_xgboost_all_scenarios.csv")
auc_tau <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2_xgboost_final.csv")
auc_tau_prev_large <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2_xgboost_prev_large.csv")
auc_tau_prev_small <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2_xgboost_prev_small.csv")

oe_tau <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/oe_tau2_XGBoost_all_scenarios.csv")
oe_tau <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/oe_tau2_xgboost.csv")
oe_tau_prev_large <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/oe_tau2_xgboost_prev_large.csv")
oe_tau_prev_small <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/oe_tau2_xgboost_prev_small.csv")

# rename models in prev data
auc_tau$Dataset <- sub("_TRUE_2$", "_TRUE", auc_tau$Dataset)
auc_tau_prev_large$Dataset <- sub("_FALSE_prevalence", "_prev_large", auc_tau_prev_large$Dataset)
auc_tau_prev_small$Dataset <- sub("_FALSE_prevalence", "_prev_small", auc_tau_prev_small$Dataset)

all_auc_tau <- rbind(auc_tau)#, auc_tau_prev_large, auc_tau_prev_small)

oe_tau$Dataset <- sub("_TRUE_2$", "_TRUE", oe_tau$Dataset)
oe_tau_prev_large$Dataset <- sub("_prevalence", "_prev_large", oe_tau_prev_large$Dataset)
oe_tau_prev_small$Dataset <- sub("_prevalence", "_prev_small", oe_tau_prev_small$Dataset)

all_oe_tau <- rbind(oe_tau)#, oe_tau_prev_large, oe_tau_prev_small)

# plot

### AUC ###

# Add Category: 100 / 500 / 1000
all_auc_tau <- all_auc_tau %>%
  filter(str_detect(Dataset, "^results_5_5_100_|^results_5_5_500_|^results_5_5_1000_|^results_8_2_100_|^results_8_2_500_|^results_8_2_1000_")) %>%
  mutate(
    size_group = case_when(
      str_detect(Dataset, "_100_") ~ "100",
      str_detect(Dataset, "_500_") ~ "500",
      str_detect(Dataset, "_1000_") ~ "1000"
    ),
    base_group = case_when(
      str_detect(Dataset, "^results_5_5_") ~ "5_5",
      str_detect(Dataset, "^results_8_2_") ~ "8_2"
    ),
    ending_type = case_when(
      str_ends(Dataset, "_FALSE") ~ "_FALSE",
      str_ends(Dataset, "_TRUE") ~ "_TRUE",
      str_ends(Dataset, "_prev_large") ~ "_prev_large",
      str_ends(Dataset, "_prev_small") ~ "_prev_small",
      TRUE ~ "Other"
    ),
    # Combine base + size to make facets
    facet_group = paste0(base_group, "_", size_group),
    
    # Order factors for plotting
    facet_group = factor(facet_group, levels = c(
      "5_5_100", "5_5_500", "5_5_1000",
      "8_2_100", "8_2_500", "8_2_1000"
    )),
    ending_type = factor(ending_type, levels = c("_FALSE", "_TRUE", "_prev_large", "_prev_small"))
  )

# Plot
ggplot(all_auc_tau, aes(x = Dataset, y = Tau2, fill = ending_type)) +
  geom_boxplot() +
  facet_wrap(~ facet_group, nrow = 2, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "_FALSE" = "lightsteelblue1",   
      "_TRUE" = "thistle",       # Green
      "_prev_large" = "palegreen3", # Orange
      "_prev_small" = "wheat1"  # Red
    ),
    name = "Type"
  ) +
  labs(title = "Boxplots of  AUC Tau2",
       x = "Dataset", y = "Tau2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )


#### try to make a bit prettier #### 

# ending_type = factor(ending_type, levels = c("_FALSE", "_TRUE", "_prev_large", "_prev_small"))

all_auc_tau <- all_auc_tau %>%
  mutate(
    facet_group = fct_recode(facet_group,
                             "5 Onco - 5 Non-onco, 100/center" = "5_5_100",
                             "5 Onco - 5 Non-onco, 500/center" = "5_5_500",
                             "5 Onco - 5 Non-onco, 1000/center" = "5_5_1000",
                             "8 Onco - 2 Non-onco, 100/center" = "8_2_100",
                             "8 Onco - 2 Non-onco, 500/center" = "8_2_500",
                             "8 Onco - 2 Non-onco, 1000/center" = "8_2_1000"
    )
  )

all_auc_tau <- all_auc_tau %>%
  mutate(
    dataset_label = case_when(
      str_ends(Dataset, "_FALSE") ~ "No higher-order variable",
      str_ends(Dataset, "_TRUE") ~ "Type of center",
      str_ends(Dataset, "_prev_large") ~ "Prevalence full training set",
      str_ends(Dataset, "_prev_small") ~ "Prevalence small training set",
      TRUE ~ Dataset  # fallback
    ),
    dataset_label = factor(dataset_label, levels = c(
      "No higher-order variable",
      "Type of center",
      "Prevalence full training set",
      "Prevalence small training set"
    ))
  )

ggplot(all_auc_tau, aes(x = dataset_label, y = Tau2, fill = ending_type)) +
  geom_boxplot() +
  facet_wrap(~ facet_group, nrow = 2, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "_FALSE" = "lightsteelblue1",    
      "_TRUE" = "thistle",       
      "_prev_large" = "palegreen3", 
      "_prev_small" = "wheat1"
    ),
    name = "Type"
  ) +
  labs(title = "Boxplots of AUC Tau2",
       x = "Design", y = "Tau2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )

########### final plot AUC ##########
### plot this one by one ###
# have one plot with the labels maybe 

# Get unique facet groups
facet_levels <- unique(all_auc_tau$facet_group)
# facet_levels <- facet_levels[6] 
# Loop through each facet group and plot
for (group in facet_levels) {
  df_subset <- filter(all_auc_tau, facet_group == group)
  
  p <- ggplot(df_subset, aes(x = dataset_label, y = Tau2, fill = ending_type)) +
    geom_boxplot() +
    scale_fill_manual(
      values = c(
        "_FALSE" = "lightsteelblue1",    
        "_TRUE" = "thistle",       
        "_prev_large" = "palegreen3", 
        "_prev_small" = "wheat1"
      ),
      labels = c(
        "_FALSE" = "No higher-order variable",
        "_TRUE" = "Center Type",
        "_prev_large" = "True Prevalence",
        "_prev_small" = "Estimated Prevalence"
      ),
      name = "Higher-order variable"
    ) +
    labs(x = "Design", y = "Tau^2") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), 
      axis.title.x = element_blank(), 
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none"
    ) + 
    ylim(0.15, 0.55)
  
  print(p)  # Display the plot
}
library(cowplot)
# Display the legend alone
legend <- get_legend(p)
plot_grid(legend)

### OE-ratio ###

# Filter data for relevant prefixes
 # all_oe_tau_subset <- all_oe_tau[grepl("^results_5_5_100_|^results_5_5_500_|^results_5_5_1000_|^results_8_2_100_|^results_8_2_500_|^results_8_2_1000_", all_oe_tau$Dataset), ]

# Add Category: 100 / 500 / 1000
all_oe_tau <- all_oe_tau %>%
  filter(str_detect(Dataset, "^results_5_5_100_|^results_5_5_500_|^results_5_5_1000_|^results_8_2_100_|^results_8_2_500_|^results_8_2_1000_")) %>%
  mutate(
    size_group = case_when(
      str_detect(Dataset, "_100_") ~ "100",
      str_detect(Dataset, "_500_") ~ "500",
      str_detect(Dataset, "_1000_") ~ "1000"
    ),
    base_group = case_when(
      str_detect(Dataset, "^results_5_5_") ~ "5_5",
      str_detect(Dataset, "^results_8_2_") ~ "8_2"
    ),
    ending_type = case_when(
      str_ends(Dataset, "_FALSE") ~ "_FALSE",
      str_ends(Dataset, "_TRUE") ~ "_TRUE",
      str_ends(Dataset, "_prev_large") ~ "_prev_large",
      str_ends(Dataset, "_prev_small") ~ "_prev_small",
      TRUE ~ "Other"
    ),
    # Combine base + size to make facets
    facet_group = paste0(base_group, "_", size_group),
    
    # Order factors for plotting
    facet_group = factor(facet_group, levels = c(
      "5_5_100", "5_5_500", "5_5_1000",
      "8_2_100", "8_2_500", "8_2_1000"
    )),
    ending_type = factor(ending_type, levels = c("_FALSE", "_TRUE", "_prev_large", "_prev_small"))
  )

# Plot
ggplot(all_oe_tau, aes(x = Dataset, y = Tau2, fill = ending_type)) +
  geom_boxplot() +
  facet_wrap(~ facet_group, nrow = 2, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "_FALSE" = "#1f77b4",      # Blue
      "_TRUE" = "#2ca02c",       # Green
      "_prev_large" = "purple", # Orange
      "_prev_small" = "pink"  # Red
    ),
    name = "Type"
  ) +
  labs(title = "Boxplots of O/E-ratio Tau2",
       x = "Dataset", y = "Tau2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )



#### final plots for OE-ratio ####

# Add Category: 100 / 500 / 1000
all_oe_tau <- all_oe_tau %>%
  filter(str_detect(Dataset, "^results_5_5_100_|^results_5_5_500_|^results_5_5_1000_|^results_8_2_100_|^results_8_2_500_|^results_8_2_1000_")) %>%
  mutate(
    size_group = case_when(
      str_detect(Dataset, "_100_") ~ "100",
      str_detect(Dataset, "_500_") ~ "500",
      str_detect(Dataset, "_1000_") ~ "1000"
    ),
    base_group = case_when(
      str_detect(Dataset, "^results_5_5_") ~ "5_5",
      str_detect(Dataset, "^results_8_2_") ~ "8_2"
    ),
    ending_type = case_when(
      str_ends(Dataset, "_FALSE") ~ "_FALSE",
      str_ends(Dataset, "_TRUE") ~ "_TRUE",
      str_ends(Dataset, "_prev_large") ~ "_prev_large",
      str_ends(Dataset, "_prev_small") ~ "_prev_small",
      TRUE ~ "Other"
    ),
    # Combine base + size to make facets
    facet_group = paste0(base_group, "_", size_group),
    
    # Order factors for plotting
    facet_group = factor(facet_group, levels = c(
      "5_5_100", "5_5_500", "5_5_1000",
      "8_2_100", "8_2_500", "8_2_1000"
    )),
    ending_type = factor(ending_type, levels = c("_FALSE", "_TRUE", "_prev_large", "_prev_small"))
  )



all_oe_tau <- all_oe_tau %>%
  mutate(
    facet_group = fct_recode(facet_group,
                             "5 Onco - 5 Non-onco, 100/center" = "5_5_100",
                             "5 Onco - 5 Non-onco, 500/center" = "5_5_500",
                             "5 Onco - 5 Non-onco, 1000/center" = "5_5_1000",
                             "8 Onco - 2 Non-onco, 100/center" = "8_2_100",
                             "8 Onco - 2 Non-onco, 500/center" = "8_2_500",
                             "8 Onco - 2 Non-onco, 1000/center" = "8_2_1000"
    )
  )

all_oe_tau <- all_oe_tau %>%
  mutate(
    dataset_label = case_when(
      str_ends(Dataset, "_FALSE") ~ "No higher-order variable",
      str_ends(Dataset, "_TRUE") ~ "Type of center",
      str_ends(Dataset, "_prev_large") ~ "Prevalence full training set",
      str_ends(Dataset, "_prev_small") ~ "Prevalence small training set",
      TRUE ~ Dataset  # fallback
    ),
    dataset_label = factor(dataset_label, levels = c(
      "No higher-order variable",
      "Type of center",
      "Prevalence full training set",
      "Prevalence small training set"
    ))
  )


# Get unique facet groups
facet_levels <- unique(all_oe_tau$facet_group)
# facet_levels <- facet_levels[6]
# Loop through each facet group and plot
for (group in facet_levels) {
  df_subset <- filter(all_oe_tau, facet_group == group)
  
  p <- ggplot(df_subset, aes(x = dataset_label, y = Tau2, fill = ending_type)) +
    geom_boxplot() +
    scale_fill_manual(
      values = c(
        "_FALSE" = "lightsteelblue1",    
        "_TRUE" = "thistle",       
        "_prev_large" = "palegreen3", 
        "_prev_small" = "wheat1"
      ),
      labels = c(
        "_FALSE" = "No higher-order variable",
        "_TRUE" = "Type of center",
        "_prev_large" = "Prevalence full training set",
        "_prev_small" = "Prevalence small training set"
      ),
      name = "Higher-order variable"
    ) +
    labs(x = "Design", y = "Tau^2") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), 
      axis.title.x = element_blank(), 
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none"
    ) + 
    ylim(0.01, 0.06)
  
  print(p)  # Display the plot
}
library(cowplot)
# Display the legend alone
legend <- get_legend(p)
plot_grid(legend)
