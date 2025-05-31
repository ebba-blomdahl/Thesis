#### Script for plotting AUC ####

# Load necessary libraries
library(ggplot2)
library(dplyr)

# define folder where the data is located 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest"

# Define which sets to import and change depenidign on which comparisment is wanted 
pattern <- "^metrics_5_5_.*\\.csv$"

# List all files matching the pattern
files_to_import <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Import and name the datasets dynamically
datasets <- lapply(files_to_import, read.csv)

# name the list elements based on file names 
names(datasets) <- gsub(".csv", "", basename(files_to_import))

# Check the imported datasets
# str(datasets)

# Combine datasets into one data frame, adding the model name as a new column
combined_data <- bind_rows(lapply(names(datasets), function(name) {
  df <- datasets[[name]]
  df$model <- name
  return(df)
}))


##### AUC ####

# Summarize the data with mean AUC and 95% CI
summary_data_AUC <- combined_data %>%
  group_by(model) %>%
  summarise(
    mean_auc = mean(AUC, na.rm = TRUE),
    se = sd(AUC, na.rm = TRUE) / sqrt(n()),            
    ci_lower = mean_auc - 1.96 * se,                   
    ci_upper = mean_auc + 1.96 * se                    
  )


ggplot(summary_data_AUC, aes(x = mean_auc, y = reorder(model, mean_auc))) +
  geom_point(size = 2) +                                            # Mean AUC points
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +  # 95% CI error bars
  theme_minimal() +
  labs(title = "AUC per Trainingset", x = "AUC", y = "Trainingset") + 
  theme(
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_blank()
  ) +
  xlim(0.89, 0.907)                                                    



##### O/E-ratio #####

# Summarize the data with mean O/E- ratio and 95% CI
summary_data_OE <- combined_data %>%
  group_by(model) %>%
  summarise(
    mean_OE = mean(O_E, na.rm = TRUE),
    se = sd(O_E, na.rm = TRUE) / sqrt(n()),           
    ci_lower = mean_OE - 1.96 * se,                  
    ci_upper = mean_OE + 1.96 * se                    
  )


ggplot(summary_data_OE, aes(x = mean_OE, y = reorder(model, mean_OE))) +
  geom_point(size = 2) +                                            # Mean AUC points
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +  # 95% CI error bars
  theme_minimal() +
  labs(title = "O/E-ratio per Trainingset", x = "O/E-ratio", y = "Trainingset") + 
  theme(
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_blank()
  ) +
  xlim(1, 1.15)   

#### Net benefit ####

# Summarize the data with mean Net Benefit and 95% CI
summary_data_nb <- combined_data %>%
  group_by(model) %>%
  summarise(
    mean_nb = mean(net_benefit, na.rm = TRUE),
    se = sd(net_benefit, na.rm = TRUE) / sqrt(n()),           
    ci_lower = mean_nb - 1.96 * se,                   
    ci_upper = mean_nb + 1.96 * se                   
  )


ggplot(summary_data_nb, aes(x = mean_nb, y = reorder(model, mean_nb))) +
  geom_point(size = 2) +                                            # Mean AUC points
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +  # 95% CI error bars
  theme_minimal() +
  labs(title = "Net Benefit per Trainingset", x = "Net Benefit", y = "Trainingset") + 
  theme(
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_blank()
  ) +
  xlim(0.296, 0.305)   
