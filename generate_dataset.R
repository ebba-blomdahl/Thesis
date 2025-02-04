#### script to generate dataset ####

# libraries
library(randomForest)

# Load the prepared dataset
load("/Users/ebbablomdahl/Thesis/Code/IOTAsynthpop_data3_prepared.RData")

# create function to generate dataset 

generate_dataset <- function(nbr_oncology = 5, nbr_non_oncology = 5, center_size = 500, center_type_predictor = TRUE){
  # nbr_oncology - how many oncology centers to include in dataset
  # nbr_non_oncology - how many non oncology centers to include in dataset
  # center_size - how many data points to include from each center 
  # center_type_predictor - if the type of center shpuld be included as a predictor (if TRUE = included)
  
  # indentify which cetners are oncology centers 
  center_type <- all_data %>% distinct(center, oncocenter)
  
  # select the correct number of centers for each type 
  oncology_centers <- center_type %>% filter(oncocenter == 1) %>% sample_n(nbr_oncology)
  non_oncology_centers <- center_type %>% filter(oncocenter == 0) %>% sample_n(nbr_non_oncology)
  
  # merge datasets for the two different types of centers 
  selected_centers <- bind_rows(oncology_centers, non_oncology_centers)
  
  # filter data to only contain the selected centers 
  dataset <- all_data %>% 
    filter(center %in% selected_centers$center) %>%
    group_by(center) %>% 
    sample_n(center_size, replace = TRUE) %>%
    ungroup()

  # remove outcome5 from dataset? 
  dataset <- dataset %>% select(-outcome5)
  
  if (center_type_predictor == FALSE){
    dataset <- dataset %>% select(-center)
  }
  
  # Impute missing values using na.roughfix() - ask about a better way to do this 
  dataset <- dataset %>%
    mutate(across(where(is.character), as.factor))
  dataset <- na.roughfix(dataset)
  
  return (dataset)
}