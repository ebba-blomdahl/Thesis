library(data.table)
library(stringr)
library(future.apply)
library(metamisc)
library(ggplot2)
library(pROC)

# Define folder path
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest"

# Define pattern for metrics files
pattern <- "^metrics_5_5_.*\\.csv$"

# List all matching files
file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  

# Function to process datasets
process_dataset <- function(df, dataset_name) {
  metrics_list <- list()
  
  # Identify model names based on column pattern
  prediction_cols <- grep("^predictions_", colnames(df), value = TRUE)
  probability_cols <- gsub("predictions_", "probabilities_", prediction_cols)  # Match probabilities
  
  for (i in seq_along(prediction_cols)) {
    model_name <- gsub("predictions_", "Model_", prediction_cols[i])  # Rename to "Model_X"
    
    for (c in unique(df$center)) {
      subdata <- df[center == c]  
      pred <- subdata[[prediction_cols[i]]]  # Predictions
      probs <- subdata[[probability_cols[i]]]  # Probabilities
      outcome <- subdata$outcome1  
      
      # Compute confusion matrix values
      TP <- sum(pred == 1 & outcome == 1)
      TN <- sum(pred == 0 & outcome == 0)
      FP <- sum(pred == 1 & outcome == 0)
      FN <- sum(pred == 0 & outcome == 1)
      
      # Compute AUC
      auc_value <- auc(outcome, probs)  
      
      # Store results
      metrics_list[[length(metrics_list) + 1]] <- list(
        Dataset = dataset_name,  
        Model = model_name,  
        center = c,  
        AUC = auc_value,  
        TP = TP,  
        TN = TN,  
        FP = FP,  
        FN = FN,  
        expect = TP + FP,  
        pos = TP + FN  
      )
    }
  }
  
  metrics <- rbindlist(metrics_list)
  return(metrics)
}

# Set up parallel processing
num_cores <- parallel::detectCores() - 1
plan(multisession, workers = num_cores)

# Read and process all datasets in parallel
all_metrics <- future_lapply(seq_along(file_list), function(i) {
  df <- fread(file_list[i])  
  process_dataset(df, dataset_params[i])  
})

# Combine all results
combined_metrics <- rbindlist(all_metrics)
write.csv(combined_metrics, "/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/metrics_all_data.csv", row.names = FALSE)

# Meta-analysis setup
sample_size <- 10000  
meta_results <- list()
tau2_values <- list()

for (dataset in unique(combined_metrics$Dataset)) {
  dataset_data <- combined_metrics[Dataset == dataset]
  
  for (model in unique(dataset_data$Model)) {
    model_data <- dataset_data[Model == model]
    
    if (nrow(model_data) > 0) {
      # Run random-effects meta-analysis
      meta_auc <- valmeta(
        measure = "cstat",
        cstat = model_data$AUC,
        O = model_data$pos,  
        E = model_data$expect,  
        N = sample_size,  
        slab = model_data$center,  
        data = model_data,  
        method = "REML"  
      )
      
      meta_results[[paste(dataset, model, sep = "_")]] <- meta_auc
      tau2_values[[paste(dataset, model, sep = "_")]] <- meta_auc$fit$tau2  
      
      # Print summary
      cat("\nDataset:", dataset, " | Model:", model, "\n")
      print(meta_auc$fit)
    } else {
      cat("\nSkipping Dataset:", dataset, " | Model:", model, " (No Data)\n")
    }
  }
}

# Save tau² values
tau2_df <- data.frame(
  Dataset = sub("_Model_.*", "", names(tau2_values)),  
  Tau2 = unlist(tau2_values)  
)

write.csv(tau2_df, "/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2.csv", row.names = FALSE)

# Plot Tau²
ggplot(tau2_df, aes(x = Dataset, y = Tau2, fill = Dataset)) +
  geom_boxplot() +
  labs(title = "Boxplot of Tau² Values Across Datasets", 
       x = "Dataset", 
       y = "Tau² (Heterogeneity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels
