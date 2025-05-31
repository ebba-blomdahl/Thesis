###### script to calculate the prevalence of the entire test set #####
# this is done for the case where 10 000 points were taken per center to the test data 
library(dplyr)

# import training set 
load("/Users/ebbablomdahl/Thesis/Thesis/datasets/train_set_10000_per_center.RData")

# calculate the prevalence based on center 
prevalence_df <- train_data %>%
  group_by(center) %>%
  summarise(
    cases = sum(outcome1),
    total = n(),
    prevalence = cases / total
  )

prevalence_df

# save prevalence in RData
# save test set 
save(prevalence_df, file = "/Users/ebbablomdahl/Thesis/Thesis/datasets/entire_train_set_prevalence_10000_per_center.RData")
