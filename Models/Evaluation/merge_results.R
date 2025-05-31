### script to merge training results from the same model ### 

# import dataset 
results_1 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/results_8_2_1000_FALSE.csv")
results_2 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/results_8_2_1000_FALSE_2.csv")

metrics_1 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/metrics_8_2_1000_FALSE.csv")
metrics_2 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/metrics_8_2_1000_FALSE_2.csv")

centers_1 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/unique_centers_8_2_1000_FALSE.csv")
centers_2 <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost_prev_small/unique_centers_8_2_1000_FALSE_2.csv")


### merge results ###

# Select columns from data2 (starting at "predictions_1")
start_col_pred <- which(colnames(results_2) == "predictions_1")  # Find column index
start_col_prob <- which(colnames(results_2) == "probabilities_1")  # First metrics column

data2_pred <- results_2[, start_col_pred:(start_col_prob - 1)]  # Keep only prediction columns
data2_prob <- results_2[, start_col_prob:ncol(results_2)]

# Rename columns in results_2 to start from "predictions_x"
num_existing_pred <- sum(grepl("^predictions_", colnames(results_1)))  
num_existing_prob <- sum(grepl("^probabilities_", colnames(results_1)))  

new_pred_colnames <- paste0("predictions_", 
                            (num_existing_pred + 1):(num_existing_pred + ncol(data2_prob)))
new_prob_colnames <- paste0("probabilities_", 
                              (num_existing_prob + 1):(num_existing_prob + ncol(data2_pred)))

colnames(data2_pred) <- new_pred_colnames
colnames(data2_prob) <- new_prob_colnames

# Merge (column-wise)
results_merged <- cbind(results_1, data2_pred, data2_prob)

# fix order of columns
# Get column names
all_cols <- colnames(results_merged)

# Identify column groups
feature_cols <- all_cols[!grepl("predictions_|probabilities_", all_cols)]  # All columns except predictions/probabilities
predictions_cols <- all_cols[grepl("^predictions_", all_cols)]  # All prediction columns
probabilities_cols <- all_cols[grepl("^probabilities_", all_cols)]  # All probability columns

# Reorder columns: Features -> Predictions -> Probabilities
new_order <- c(feature_cols, predictions_cols, probabilities_cols)

# Apply the new order
results_merged <- results_merged[, new_order]

# Check new column order
colnames(results_merged)

# save final results
write.csv(results_merged, file = "Results/XGBoost_prev_small/results_8_2_1000_FALSE.csv", row.names = FALSE)

### merge metrics ### 

merged_metrics <- rbind(metrics_1, metrics_2)

# Check the result
print(merged_metrics)

# save final results 
write.csv(merged_metrics, file = "Results/XGBoost_prev_small/metrics_8_2_1000_FALSE.csv", row.names = FALSE)

### merge centers ###

merged_centers <- cbind(centers_1, centers_2)
colnames(merged_centers) <- paste0("iteration_", seq_len(ncol(merged_centers)))

# save final results 
write.csv(merged_centers, file = "Results/XGBoost_prev_small/unique_centers_8_2_1000_FALSE.csv", row.names = FALSE)

