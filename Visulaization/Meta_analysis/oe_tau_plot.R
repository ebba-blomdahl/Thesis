library(data.table)
library(stringr)
library(future.apply)
library(metamisc)
library(ggplot2)

# for o/e ratio--use the metrics already saved
combined_metrics<-read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/metrics_all_data_xgboost_all_scenarios.csv")


# Assume sample size per center is known
sample_size <- 10000  

# Initialize lists for storing meta-analysis results
meta_results <- list()
tau2_values <- list()

# Loop over datasets
for (dataset in unique(combined_metrics$Dataset)) {
  
  # Subset data for the current dataset
  dataset_data <- combined_metrics[combined_metrics$Dataset == dataset, ]
  
  # Loop over models within this dataset
  for (model in unique(dataset_data$Model)) {
    
    # Subset data for the current model within this dataset
    model_data <- dataset_data[dataset_data$Model == model,]
    
    # Ensure we have data for this model in this dataset
    if (nrow(model_data) > 0) {
      # Compute standard error for AUC
      #model_data$AUC_se <- ccalc(model_data$AUC, N = sample_size, O = model_data$pos)$theta.se
      
      # Run random-effects meta-analysis for OE
      meta_oe <- valmeta(
        measure = "OE", 
        O = model_data$pos, 
        E = model_data$expect,
        N = sample_size,
        slab = model_data$center,
        data=model_data,
        method = "REML"
      )
      
      # Store results
      meta_results[[paste(dataset, model, sep = "_")]] <- meta_oe
      tau2_values[[paste(dataset, model, sep = "_")]] <- meta_oe$fit$tau2
      
      # Print summary
      cat("\nDataset:", dataset, " | Model:", model, "\n")
      print(meta_oe$fit)
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

write.csv(tau2_df,"/Users/ebbablomdahl/Thesis/Thesis/Results/meta_analysis/oe_tau2_XGBoost_all_scenarios.csv",row.names = FALSE)

ggplot(tau2_df, aes(x = Dataset, y = Tau2, fill = Dataset)) +
  geom_boxplot() +
  labs(title = "Boxplot of Tau² Values Across Datasets", 
       x = "Dataset", 
       y = "Tau² (Heterogeneity)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels









