#### script to generate datasets ####

# libraries
library(randomForest)
library(dplyr)

# Load the training set. WARNING! change which traingset manually here 
# so it matches the test set - much more efficient in running time than 
# having it inside the function 
load("/Users/ebbablomdahl/Thesis/Thesis/datasets/train_set_10000_per_center.RData") 
load("/Users/ebbablomdahl/Thesis/Thesis/datasets/test_set_10000_per_center.RData")
# create function to generate dataset 

generate_train_set <- function(nbr_oncology = 5, nbr_non_oncology = 5, center_size = 500, center_type_predictor = TRUE){
  # nbr_oncology - how many oncology centers to include in dataset
  # nbr_non_oncology - how many non oncology centers to include in dataset
  # center_size - how many data points to include from each center 
  # center_type_predictor - if the type of center should be included as a predictor (if TRUE = included)
  
  # identify which centers are oncology centers 
  center_type <- train_data %>% distinct(center, oncocenter)
  
  # select the correct number of centers for each type 
  oncology_centers <- center_type %>% filter(oncocenter == 1) %>% sample_n(nbr_oncology)
  non_oncology_centers <- center_type %>% filter(oncocenter == 0) %>% sample_n(nbr_non_oncology)
  
  # merge datasets for the two different types of centers 
  selected_centers <- bind_rows(oncology_centers, non_oncology_centers) 
  
  # filter data to only contain the selected centers 
  train_set <- train_data %>% 
    filter(center %in% selected_centers$center) %>%
    group_by(center) %>% 
    sample_n(center_size, replace = TRUE) %>%
    ungroup()
  
  #### interpolate the missing values of lCA125 and CA125 ####
  # transform lCA125 using log(x+1)
  train_set <- train_set %>%
    mutate(lCA125_trans = ifelse(!is.na(lCA125), log(lCA125+1), NA))
  
  # removed onconcenter according to paper since it is not used in all train_sets for predicting 
  # fit regression model to predict the missing values 
  model_lCA125 <- lm(lCA125_trans ~ Age + bilateral + llesdmax + loc10 + propsol + papnr + Ascites + Shadows + 
                       colscore + wallreg + papflow + oncocenter, data = train_set, na.action = na.exclude)
  
  # predict the missing values 
  train_set$lCA125_trans[is.na(train_set$lCA125_trans)] <- predict(model_lCA125, newdata = train_set[is.na(train_set$lCA125_trans),])
  
  # back transform to the original scale 
  train_set <- train_set %>%
    mutate(lCA125_imputed = exp(lCA125_trans) - 1)
  
  # for the NA CA125 add the exp of the imputed value 
  train_set <- train_set %>%
    mutate(CA125= ifelse(is.na(CA125), exp(lCA125_imputed), CA125)) 
  
  # add the value of lCA125_imputed to the missing values of lCA125
  train_set <- train_set %>%
    mutate(lCA125 = ifelse(is.na(lCA125), lCA125_imputed, lCA125))
  
  # remove extra columns of data 
  train_set <- train_set %>% select(-lCA125_trans, -lCA125_imputed)
  
  # remove center type as predictor - not sure it is the smartest to have it here
  if (center_type_predictor == FALSE){
    train_set <- train_set %>% select(-oncocenter)
  }
  
  ### do interpolation for test set with the same model ### 
  # transform lCA125 using log(x+1)
  test_set <- test_data %>%
    mutate(lCA125_trans = ifelse(!is.na(lCA125), log(lCA125+1), NA))
  
  # predict the missing values 
  test_set$lCA125_trans[is.na(test_set$lCA125_trans)] <- predict(model_lCA125, newdata = test_set[is.na(test_set$lCA125_trans),])
  
  # back transform to the original scale 
  test_set <- test_set %>%
    mutate(lCA125_imputed = exp(lCA125_trans) - 1)
  
  # for the NA CA125 add the exp of the imputed value 
  test_set <- test_set %>%
    mutate(CA125= ifelse(is.na(CA125), exp(lCA125_imputed), CA125)) 
  
  # add the value of lCA125_imputed to the missing values of lCA125
  test_set <- test_set %>%
    mutate(lCA125 = ifelse(is.na(lCA125), lCA125_imputed, lCA125))
  
  # remove extra columns of data 
  test_set <- test_set %>% select(-lCA125_trans, -lCA125_imputed)
  
  # remove center type as predictor - not sure it is the smartest to have it here
  if (center_type_predictor == FALSE){
    test_set <- test_set %>% select(-oncocenter)
  }
  
  return (list(train_set = train_set, test_set = test_set))
}