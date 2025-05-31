### values for tables ###

results <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest/results_8_2_500_TRUE.csv")

source("evaluation_metrics.R")

metrics <- evaluation_metrics(results$outcome1, results$predictions_1, results$probabilities_1)

# Create an empty list to store the results
all_metrics <- vector("list", 100)

# Loop through 1 to 100
for (i in 1:100) {
  # Dynamically get the column names
  preds <- results[[paste0("predictions_", i)]]
  probs <- results[[paste0("probabilities_", i)]]
  
  # Call your custom evaluation function
  metrics <- evaluation_metrics(results$outcome1, preds, probs)
  
  # Store the result as a named vector
  all_metrics[[i]] <- metrics
}

# Convert the list of named vectors to a data frame
metrics_df <- do.call(rbind, all_metrics)
metrics <- as.data.frame(metrics_df)

metrics <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest_prev_small/metrics_8_2_1000_FALSE_prevalence.csv")

mean_auc <- mean(metrics$AUC)
mean_auc
sd_auc <- sd(metrics$AUC)
sd_auc

mean_oe <- mean(metrics$O_E)
mean_oe
sd_oe <- sd(metrics$O_E)
sd_oe

mean_nb <- mean(metrics$net_benefit)
mean_nb
sd_nb <- sd(metrics$net_benefit)
sd_nb

