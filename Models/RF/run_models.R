#### function to run model x times and save all the results #### 

# function to import 
source("random_forest_model.R") # import the model that will be used - change depending on model
source("evaluation_metrics.R") # import function for evaluation metrics 
# source("generate_train_set.R") # import function to generate train and test set 
source("generate_train_set_prevalence.R") # generate train set with prevalence 
source("clean_data_rf.R") # import data to clean and prepare dataset - change depending on model? 

# decide train set parameters 
nbr_onco <- 8
nbr_non_onco <- 2
center_size <-1000
center_type_predictor <- FALSE
subfolder <- "Results/random_forest_prev_small" # change subfolder depending on model
prevalence_large = FALSE
prevalence_small = TRUE

run_model <- function(nbr_onco = nbr_onco, nbr_non_onco = nbr_non_onco, center_size = center_size, center_type_predictor = center_type_predictor, prevalence_large = prevalence_large, prevalence_small = prevalence_small){ 
  # nbr of iterations
  n_iter = 100
  
  # Initialize empty matrices to save prediction results
  predictions_mat <- matrix(NA, nrow = nrow(test_data), ncol = n_iter) 
  probabilities_mat <- matrix(NA, nrow = nrow(test_data), ncol = n_iter)
  unique_centers_mat <- matrix(NA, nrow = nbr_onco + nbr_non_onco, ncol = n_iter)
  
  # create df to save the metrics for each run 
  auc_values <- numeric(n_iter)  # Store AUC values
  oe_ratios <- numeric(n_iter)  # Store O/E ratios
  net_benefits <- numeric(n_iter)  # Store net benefit values
  
  # loop over 100 training sets
  for (n in 1:n_iter){
    print(n) # print to see which iteration
    # import training set and test set
    data_gen <- generate_train_set(nbr_oncology = nbr_onco, nbr_non_oncology = nbr_non_onco, center_size = center_size, center_type_predictor = center_type_predictor)
    
    # clean and prepare data with function
    data <- prepare_data(data_gen$train_set, data_gen$test_set)
    
    # Store unique centers for this iteration (always 10)
    unique_centers_mat[, n] <- unique(data$train_set$center)
    
    # fetch final model 
    model <- final_model(data$train_set)
    
    # save probabilities and binary predictions in matrix 
    # Get predictions and probabilities
    predictions_mat[, n] <- as.numeric(predict(model, data$test_set %>% select(-center))) -1
    probabilities_mat[, n] <- predict(model, data$test_set %>% select(-center), type = "prob")[,2]

    # get evaluation metrics
    metrics <- evaluation_metrics(data$test_set$outcome1, predictions_mat[, n], probabilities_mat[, n])
    
    # save evaluation metrics 
    auc_values[n] <- metrics["AUC"]
    oe_ratios[n] <- metrics["O_E"]
    net_benefits[n] <- metrics["net_benefit"]
  }
  
  ### Save unique centers for all iterations ###
  # Convert matrix to dataframe
  unique_centers_df <- as.data.frame(unique_centers_mat)
  
  # Rename columns to indicate iteration numbers
  colnames(unique_centers_df) <- paste0("iteration_", 1:n_iter)
  
  ### save final test_set ###
  # Convert matrices to a dataframe with meaningful column names
  predictions_df <- as.data.frame(predictions_mat)
  probabilities_df <- as.data.frame(probabilities_mat)
  
  colnames(predictions_df) <- paste0("predictions_", 1:n_iter)
  colnames(probabilities_df) <- paste0("probabilities_", 1:n_iter)
  
  # Combine results with test_set
  final_results <- cbind(data$test_set, predictions_df, probabilities_df)
  
  # Create a dataframe for the evaluation metrics
  metrics_df <- data.frame(
    AUC = auc_values,
    O_E = oe_ratios,
    net_benefit = net_benefits
  )
  
  ### save results locally ### 
  # Define the file name dynamically
  results_filename <- paste0(subfolder, "/", "results_", nbr_onco, "_", nbr_non_onco, "_", center_size, "_", center_type_predictor, "_prevalence", "_2", ".csv")
  metrics_filename <- paste0(subfolder, "/", "metrics_", nbr_onco, "_", nbr_non_onco, "_", center_size, "_", center_type_predictor, "_prevalence", "_2", ".csv")
  unique_centers_filename <- paste0(subfolder, "/", "unique_centers_", nbr_onco, "_", nbr_non_onco, "_", center_size, "_", center_type_predictor, "_prevalence", "_2", ".csv")
  
  # Save the unique centers dataframe
  write.csv(unique_centers_df, file = unique_centers_filename, row.names = FALSE)
  
  # save final results, containing the test set, predictions and probabailities 
  write.csv(final_results, file = results_filename, row.names = FALSE)
  
  # save the meterics
  write.csv(metrics_df, file = metrics_filename, row.names = FALSE)
    
}

# run everything 
run_model(nbr_onco = nbr_onco, nbr_non_onco = nbr_non_onco, center_size = center_size, center_type_predictor = center_type_predictor, prevalence_large = prevalence_large, prevalence_small = prevalence_small)
