library(data.table)
library(stringr)
library(future.apply)
library(metamisc)
library(ggplot2)
library(pROC)

# List all dataset file paths
# define folder where the data is located 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost"

# Define which sets to import and change depenidign on which comparisment is wanted 
pattern <- "^results_.*\\.csv$"

# List all files matching the pattern
file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  # Extracts filename without extension

process_dataset <- function(df, dataset_name) {
  metrics_list <- list()
  # for random forest 
  # df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)
  
  models <- paste0("Model_", 1:100)
  # Identify model names based on column pattern
  prediction_cols <- grep("predictions_", colnames(df), value = TRUE)
  probability_cols <- gsub("predictions_", "probabilities_", prediction_cols)  # Match probabilities
  
  j = 1
  
  for (model in models) {
    for (c in unique(df$center)) {
      subdata <- df[center == c]  # Subset data for center
      probs<-subdata[[probability_cols[j]]] # probabilities
      pred <- subdata[[prediction_cols[j]]]  # Round predictions
      outcome <- subdata$outcome1  # True outcomes
      
      ### calculate the tp/fp/tn/fn
      TP <- sum(pred == 1 & outcome == 1)
      TN <- sum(pred == 0 & outcome == 0)
      FP <- sum(pred == 1 & outcome == 0)
      FN <- sum(pred == 0 & outcome == 1)
      expect <- sum(probs)
      
      auc_value <- auc(outcome, probs)  # Compute AUC
      
      metrics_list[[length(metrics_list) + 1]] <- list(
        Dataset = dataset_name,  # Store dataset parameter name
        Model = model,
        center = c,
        AUC = auc_value,
        TP = TP,
        TN = TN,
        FP = FP,
        FN = FN,
        # expect = TP + FP, 
        expect = expect,
        pos = TP + FN
      )
    }
    print(j)
    j = j+1
  }
  
  metrics <- rbindlist(metrics_list)
  return(metrics)
}

# Set up parallel processing
num_cores <- parallel::detectCores() - 1
plan(multisession, workers = num_cores)

# Read and process all datasets in parallel
# all_metrics <- lapply(seq_along(file_list), function(i) {
#   df <- fread(file_list[i])  # Read dataset
#   process_dataset(df, dataset_params[i])  # Process it with dataset name
# })


# Initialize an empty list to store results
all_metrics <- list()

# Process files one by one
for (i in seq_along(file_list)) {
  cat("\nProcessing file:", i, "-", file_list[i], "\n")  # Debugging output
  
  df <- fread(file_list[i])  # Read dataset
  
  if (is.null(dataset_params[i])) {
    cat("Skipping file:", file_list[i], "due to missing dataset name.\n")
    next  # Skip to the next iteration
  }
  
  result <- process_dataset(df, dataset_params[i])  # Process the dataset
  
  all_metrics[[i]] <- result  # Store the result
}

# Combine all datasets' results
combined_metrics <- rbindlist(all_metrics)
combined_metrics
#### save the metrics with tp/tn/fp/fn/o/e/auc--- for oe ratio and net benefit could be used directly
write.csv(combined_metrics,"/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/metrics_all_data_xgboost_all_scenarios.csv",row.names = FALSE)
# combined_metrics<-read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/metrics_all_data_xgboost_new.csv")
# Assume sample size per center is known
sample_size <- 10000

# Initialize lists for storing meta-analysis results
meta_results <- list()
tau2_values <- list()

# Loop over datasets
for (dataset in unique(combined_metrics$Dataset)) {
  
  # Subset data for the current dataset
  dataset_data <- combined_metrics[combined_metrics$Dataset == dataset,]
  
  # Loop over models within this dataset
  for (model in unique(dataset_data$Model)) {
    
    # Subset data for the current model within this dataset
    model_data <- dataset_data[dataset_data$Model == model,]
    
    # Ensure we have data for this model in this dataset
    if (nrow(model_data) > 0) {
      # Compute standard error for AUC
      #model_data$AUC_se <- ccalc(model_data$AUC, N = sample_size, O = model_data$pos)$theta.se
      
      # Run random-effects meta-analysis for AUC
      meta_auc <- valmeta(
        measure = "cstat",
        cstat=model_data$AUC,
        O = model_data$pos, 
        E = model_data$expect,
        N = sample_size,
        slab=model_data$center,
        data=model_data,
        method = "REML"
      )
      
      # Store results
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



# Convert tau2_values list into a data frame
tau2_df <- data.frame(
  Dataset = sub("_Model_.*", "", names(tau2_values)),  # Extract dataset name
  Tau2 = unlist(tau2_values)  # Extract numeric tau² values
)


write.csv(tau2_df,"/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/auc_tau2_xgboost_all_scenarios.csv",row.names = FALSE)

ggplot(tau2_df, aes(x = Dataset, y = Tau2, fill = Dataset)) +
  geom_boxplot() +
  labs(title = "Boxplot of Tau² Values Across Datasets", 
       x = "Dataset", 
       y = "Tau² (Heterogeneity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels






