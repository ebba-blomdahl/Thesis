#### script for final XGBoost model ####
library(rBayesianOptimization)
library(xgboost)

# Function to optimize XGBoost hyperparameters
final_model <- function(train_data) {
  
  ## bayesain optimization
  objective_function <- function(eta, nrounds, max_depth, min_child_weight) {
    
    # Transform eta from logarithmic scale
    eta_value <- 2^eta
    print(paste("eta_value:", eta_value))  # Check the eta value
    
    params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      eta = eta_value,
      max_depth = as.integer(max_depth),  # Set default max_depth or adjust as needed
      min_child_weight = min_child_weight,  # Set default min_child_weight or adjust
      subsample = 0.8,  # Set default subsample or adjust
      colsample_bytree = 0.8,  # Set default colsample_bytree or adjust
      #gamma = gamma,  # Set default gamma or adjust,
      gamma = 0
      # lambda = 1,   # L2 regularization (ridge)
      # alpha = 0.1   # L1 regularization (lasso)
    )
    
    # Perform 5-fold cross-validation
    cv_results <- xgb.cv(
      params = params,
      data = train_data,# data$train_set,
      nrounds = as.integer(nrounds),
      nfold = 3,
      verbose = 0,
      early_stopping_rounds = 10
    )
    
    # Return the best AUC score (maximize)
    list(Score = max(cv_results$evaluation_log$test_auc_mean))
  }
  
  
  # run the actual optimization 
  opt_results <- BayesianOptimization(
    FUN = objective_function,
    bounds = list(
      eta = c(-10, 0),  # Log scale for eta
      nrounds = c(1, 500),  # nrounds range
      max_depth = c(3,6),
      min_child_weight = c(3,7)
      # gamma = c(0,5)
    ),
    init_points = 5,  
    n_iter = 10,      
    acq = "ucb",        
    verbose = TRUE
  )
  
  # train the final model 
  best_params <- opt_results$Best_Par
  
  final_model <- xgboost(
    data = train_data,
    max_depth = as.integer(best_params[3]),
    eta = 2^(best_params[1]), # eta
    nrounds = as.integer(best_params[2]),
    # gamma = as.integer(best_params[5]),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = as.integer(best_params[4]),
    subsample = 0.8,
    # lambda = 1,   # L2 regularization (ridge)
    # alpha = 0.1,   # L1 regularization (lasso)
    objective = "binary:logistic",
    eval_metric = "auc"
  )
  
  return(final_model)
}
