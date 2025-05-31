### function to prepare the train_datasets for random forest ###

# Function to prepare the train_dataset
prepare_data <- function(train_data, test_data) {
  # drop unecessary columns 
  train_data <- train_data %>% select(-Echogenicity, -outcome5, -CA125)#, -cases, -total)
  test_data <- test_data %>% select(-Echogenicity, -outcome5, -CA125)# , -cases, -total)
  
  # prepare for train data 
  train_data$outcome1 <- as.factor(train_data$outcome1)
  train_data$bilateral <- as.factor(train_data$bilateral)
  train_data$loc10 <- as.factor(train_data$loc10)
  train_data$Ascites <- as.factor(train_data$Ascites)
  train_data$Shadows <- as.factor(train_data$Shadows)
  train_data$wallreg <- as.factor(train_data$wallreg)
  train_data$papnr <- as.factor(train_data$papnr)
  train_data$colscore <- as.factor(train_data$colscore)
  train_data$papflow <- as.factor(train_data$papflow)
  
  if("oncocenter" %in% colnames(train_data)) {
    train_data$oncocenter <- as.factor(train_data$oncocenter)
  }
  
  # Apply make.names to ensure factor levels are valid
  train_data[] <- lapply(train_data, function(x) {
    if (is.factor(x)) {
      levels(x) <- make.names(levels(x))
    }
    return(x)
  })
  
  # prepare for test data 
  test_data$outcome1 <- as.factor(test_data$outcome1)
  test_data$bilateral <- as.factor(test_data$bilateral)
  test_data$loc10 <- as.factor(test_data$loc10)
  test_data$Ascites <- as.factor(test_data$Ascites)
  test_data$Shadows <- as.factor(test_data$Shadows)
  test_data$wallreg <- as.factor(test_data$wallreg)
  test_data$papnr <- as.factor(test_data$papnr)
  test_data$colscore <- as.factor(test_data$colscore)
  test_data$papflow <- as.factor(test_data$papflow)
  
  if("oncocenter" %in% colnames(test_data)) {
    test_data$oncocenter <- as.factor(test_data$oncocenter)
  }
  
  # Apply make.names to ensure factor levels are valid
  test_data[] <- lapply(test_data, function(x) {
    if (is.factor(x)) {
      levels(x) <- make.names(levels(x))
    }
    return(x)
  })
  
  return(list(train_set = train_data, test_set = test_data))
}
