#### script for XGBoost model #### 
library(xgboost)
library(caret)
library(rBayesianOptimization)
library(pROC)
source("generate_train_set.R") 
source("clean_data_xgboost.R")

# Function to optimize the XGBoost hyperparameters 
final_model <- function(train_data) {
  return(final_model)
}
# import data sets
data_gen <- generate_train_set(nbr_oncology = 5, nbr_non_oncology = 5, center_size = 100, center_type_predictor = TRUE)

# clean and prepare data with function
data <- prepare_data(data_gen$train_set, data_gen$test_set)



### to the training

objective_function <- function(eta, nrounds, max_depth, min_child_weight) {
  
  # Transform eta from logarithmic scale
  eta_value <- 2^eta
  # Split training data into 80% train, 20% test
  
  train_indices <- sample(1:nrow(data$train_set), size = 0.8 * nrow(data$train_set))
  
  train_data <- data$train_set[train_indices, ]
  test_data <- data$train_set[-train_indices, ]
  
  # Prepare feature matrices and labels
  train_X <- train_data[, !names(train_data) %in% "outcome1"]
  train_y <- train_data$outcome1
  
  test_X <- test_data[, !names(test_data) %in% "outcome1"]
  test_y <- test_data$outcome1
  
  train_matrix <- xgb.DMatrix(data = as.matrix(train_X), label = train_y)
  test_matrix <- xgb.DMatrix(data = as.matrix(test_X), label = test_y)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = eta_value,
    max_depth = as.integer(max_depth),
    min_child_weight = min_child_weight,
    subsample = 0.8,
    colsample_bytree = 0.8,
    gamma = 0
  )
  
  # Train XGBoost model on training split
  model <- xgboost(
    data = train_matrix,
    params = params,
    nrounds = as.integer(nrounds),
    verbose = 0
  )
  
  # Make predictions on the 20% held-out test data
  prob <- predict(model, test_matrix)
  
  # Compute AUC
  auc_value <- pROC::auc(test_y, prob)
  
  # Return AUC as the optimization score
  return(list(Score = auc_value))
}


## bayesain optimization
objective_function <- function(eta, nrounds, max_depth, min_child_weight) {
  
  # Transform eta from logarithmic scale
  eta_value <- 2^eta
  print(paste("eta_value:", eta_value))  
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = eta_value,
    max_depth = as.integer(max_depth),  
    min_child_weight = min_child_weight,  
    subsample = 0.8, 
    colsample_bytree = 0.8, 
    gamma = 0  
  )
  
  # Perform 5-fold cross-validation
  cv_results <- xgb.cv(
    params = params,
    data = data$train_set,
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
  ),
  init_points = 5,  
  n_iter = 10,    
  acq = "ucb",       
  verbose = TRUE
)

# train the final model 
best_params <- opt_results$Best_Par

final_model <- xgboost(
  data = data$train_set,
  max_depth = as.integer(best_params[3]),
  eta = 2^(best_params[1]), # eta
  nrounds = as.integer(best_params[2]),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = as.integer(best_params[4]),
  subsample = 0.8,
  objective = "binary:logistic",
  eval_metric = "auc"
)


# Make predictions
prob <- predict(final_model, data$test_set)
pred <- ifelse(prob > 0.5, 1, 0)

# Model evaluation
conf_matrix <- confusionMatrix(as.factor(pred), as.factor(data_gen$test_set$outcome1))
print(conf_matrix)

# Output AUC score
auc <- auc(data_gen$test_set$outcome1, prob)
print(paste("AUC:", auc))
roc_curve <- roc(data_gen$test_set$outcome1, prob)
roc_curve

plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for XGBoost Model")

