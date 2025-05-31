##### this script is for generating the test sets #####

# do some different sizes, 1000, 10 000, 100 000 

# libraries
library(randomForest)
library(dplyr)

# Load the prepared dataset
load("/Users/ebbablomdahl/Thesis/Code/IOTAsynthpop_data3_prepared.RData")

n <- 100000 # size of center per test set 1000, 10 000, 100 000

# remove predictors from dataset that should not be there
data_trans <- all_data %>% select(-trueprob_lr1, -trueprob_lr2, -trueprob_rf1, -trueprob_rf2, -out_lr1, -out_lr2, -out_rf1, -out_rf2, -lCAmi)

#### generate the test and train set ####

# Add a unique row ID 
data_trans <- data_trans %>%
  mutate(temp_id = row_number())

# select n random samples from each center for test data 
test_data <- data_trans %>%
  group_by(center) %>%
  slice_sample(n = n) %>%
  ungroup()

# remove selected patients from training dataset 
train_data <- anti_join(data_trans, test_data, by = colnames(df)) # make sure not to remove duplicates 

# remove temporary id 
train_data <- train_data %>% select(-temp_id)
test_data <- test_data %>% select(-temp_id)

#### Save test set and training set ####

# save test set 
save(test_data, file = "/Users/ebbablomdahl/Thesis/Thesis/datasets/test_set_100000_per_center.RData")

# save training set 
save(train_data, file = "/Users/ebbablomdahl/Thesis/Thesis/datasets/train_set_100000_per_center.RData")




