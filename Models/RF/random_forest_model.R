#### script for final random forest model #### 
library(rBayesianOptimization)

# Function to optimize Random Forest hyperparameters
final_model <- function(train_data) {
  train_x <- train_data %>% select(-outcome1, -center)
  train_y <- train_data$outcome1
  
  rf_eval <- function(mtry, nodesize, sampsize) {
    mtry <- round(mtry)
    nodesize <- round(nodesize)
    sampsize <- round(sampsize * nrow(train_x))
    
    rf_model <- randomForest(
      x = train_x, y = train_y, mtry = mtry, 
      nodesize = nodesize, sampsize = sampsize, ntree = 200
    )
    
    probs <- predict(rf_model, train_x, type = "prob")[,2]
    auc_value <- auc(train_y, probs)
    return(list(Score = auc_value, Pred = 0))
  }
  
  bounds <- list(
    mtry = c(1, sqrt(ncol(train_x))),
    nodesize = c(1, 15),
    sampsize = c(0.7, 1.0)
  )
  
  opt_results <- BayesianOptimization(
    FUN = rf_eval, bounds = bounds, init_points = 5, n_iter = 10, 
    acq = "ucb", kappa = 2.5
  )
  
  best_params <- opt_results$Best_Par
  
  final_model <- randomForest(
    x = train_x, 
    y = train_y, 
    mtry = round(best_params[["mtry"]]), 
    nodesize = round(best_params[["nodesize"]]), 
    sampsize = round(best_params[["sampsize"]] * nrow(train_x)),
    ntree = 1000, 
    importance = TRUE
  )
  
  return(final_model)
}