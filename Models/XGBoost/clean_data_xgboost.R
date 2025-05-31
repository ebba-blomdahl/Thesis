### function to prepare the dataset for XGBoost ###
prepare_data <- function(train_data, test_data) {
  # drop unecessary columns 
  train_data <- train_data %>% select(-Echogenicity, -outcome5, -total, -cases) # , -CA125, -propsol, -lCA125, -colscore)#, -total, -cases)
  test_data <- test_data %>% select(-Echogenicity, -outcome5, -total, -cases) # , -CA125, -propsol, -lCA125, -colscore)#, -total, -cases)
  
  # train_data$oncocenter_num <- as.numeric(train_data$oncocenter)
  # train_data$oncocenter_CA125 <- train_data$oncocenter * train_data$CA125
  # train_data$oncocenter_propsol <- train_data$oncocenter * train_data$propsol
  # 
  # test_data$oncocenter_num <- as.numeric(test_data$oncocenter)
  # test_data$oncocenter_CA125 <- test_data$oncocenter * test_data$CA125
  # test_data$oncocenter_propsol <- test_data$oncocenter * test_data$propsol
  
  # convert dataset to matrices 
  train_x <- as.matrix(train_data %>% select(-outcome1, -center))
  train_y <- train_data$outcome1
  test_x <- as.matrix(test_data %>% select(-outcome1, -center))
  test_y <- test_data$outcome1
  
  # Convert to DMatrix (XGBoost's optimized data format)
  dtrain <- xgb.DMatrix(data = train_x, label = train_y)
  dtest <- xgb.DMatrix(data = test_x, label = test_y)
  
  return(list(train_set = dtrain, test_set = dtest))
}