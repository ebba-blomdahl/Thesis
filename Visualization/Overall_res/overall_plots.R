#### script to plot some overall results ####
library(pROC)
library(dplyr)
library(ggplot2)

# List all dataset file paths
# define folder where the data is located 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest"

# Define which sets to import and change depenidign on which comparisment is wanted 
pattern <- "^results_.*\\.csv$"

# List all files matching the pattern
file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  # Extracts filename without extension

# import center info
center_info <- read.csv("center_info.csv")

# plot AUC for onco and non onco for models including cneter type and not 
calc_auc <- function(df, dataset_name){
  models <- paste0("Model_", 1:100)
  results <- data.table()
  probability_cols <- grep("probabilities_", colnames(subdata), value = TRUE)
  
  if (!"oncocenter" %in% colnames(df)){
    df <- df %>%
      left_join(center_info, by = "center")
  }
  
  # Convert outcome to numeric for random forest
  df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)
  
  for(type in unique(df$oncocenter)){
    subdata <- df[oncocenter == type]
    outcome <- subdata$outcome1
    N <- length(outcome)
    
    for(j in 1:100){
      probs<-subdata[[probability_cols[j]]] # probabilities
      auc_value <- auc(outcome, probs)  # Compute AUC
      
      results <- rbind(results, data.table(
        Scenario = dataset_name,
        AUC = auc_value,
        Model = models[j],
        Center_type = type
      ))
    }
  }
  return(results)
}


# Run for all files
all_net_benefits <- rbindlist(lapply(seq_along(file_list), function(i) {
  cat("\nProcessing file:", i, "-", file_list[i], "\n")
  df <- fread(file_list[i])
  calc_auc(df, dataset_params[i])
}))


# plot
# Summary per group
df_summary <- all_net_benefits %>%
  group_by(Scenario, Center_type) %>%
  summarise(
    auc_mean = mean(AUC),
    auc_lower = mean(AUC) - 1.96 * sd(AUC) / sqrt(n()),
    auc_upper = mean(AUC) + 1.96 * sd(AUC) / sqrt(n()),
    auc_sd = sd(AUC),
    .groups = "drop"
  )

ggplot(df_summary, aes(x = auc_mean, y = Scenario)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = auc_lower, xmax = auc_upper), height = 0.2) +
  facet_grid(scales = "free_y", space = "free_y") +
  theme_minimal() +
  xlab("AUC") +
  ylab("Model")

# Add diamonds using geom_point with shape
ggplot(df_summary, aes(x = auc_mean, y = Scenario)) +
  geom_point(shape = 18, size = 4) +  # shape 18 is a filled diamond
  geom_errorbarh(aes(xmin = auc_lower, xmax = auc_upper), height = 0.1) +
  facet_grid( scales = "free_y", space = "free_y") +
  theme_minimal() +
  xlab("AUC") +
  ylab("Model")
