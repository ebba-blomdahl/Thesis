#### calculate net benefit #### 
# get the share of centers with a beneficial net benefit

library(data.table)
library(stringr)
library(ggplot2)

### import data ###
# List all dataset file paths
# define folder where the data is located 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost"

# Define which sets to import and change depenidign on which comparisment is wanted 
pattern <- "^results_.*\\.csv$"

# List all files matching the pattern
file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  # Extracts filename without extension

# function to calculate the net benefit per center

calc_net_benefit <- function(df, dataset_name){
  threshold <- 0.1
  models <- paste0("Model_", 1:100)
  results <- data.table()
  
  # for random forest 
  df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)
  
  # for each model 
    for(j in 1:100){
      prob_col <- paste0("probabilities_", j)
    # for each center 
      for(c in unique(df$center)){
        subdata <- df[center == c]  # Subset data for center
        probs <- subdata[[prob_col]]
        outcome <- subdata$outcome1
        
        # calculate the net benefit per center 
        N <- length(outcome)
        pred_pos <- probs >= threshold
        TP <- sum(pred_pos & (outcome == 1))
        FP <- sum(pred_pos & (outcome == 0))
        net_benefit_value <- (TP/N) - (FP/N) * (threshold/(1-threshold))
        
        results <- rbind(results, data.table(
          Dataset = dataset_name, 
          Model = models[j],
          center = c,
          NB = net_benefit_value
        ))
        
      }
    }
  return(results)
}

calc_net_benefit <- function(df, dataset_name){
  threshold <- 0.1
  models <- paste0("Model_", 1:100)
  results <- data.table()
  
  # Convert outcome to numeric for random forest
  # df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)
  
  # # Prevalence and NB of treat all - prevalence in the whole test set 
  # prevalence <- mean(df$outcome1)
  # nb_treat_all <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
  
  for(c in unique(df$center)){
    subdata <- df[center == c]
    outcome <- subdata$outcome1
    N <- length(outcome)
    
    # Prevalence and NB of treat all - prevalence per center
    prevalence <- mean(outcome)
    nb_treat_all <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
    
    for(j in 1:100){
      prob_col <- paste0("probabilities_", j)
      probs <- subdata[[prob_col]]
      
      pred_pos <- probs >= threshold
      TP <- sum(pred_pos & (outcome == 1))
      FP <- sum(pred_pos & (outcome == 0))
      net_benefit_value <- (TP/N) - (FP/N) * (threshold / (1 - threshold))
      
      results <- rbind(results, data.table(
        Dataset = dataset_name,
        Model = models[j],
        center = c,
        NB = net_benefit_value,
        NB_treat_all = nb_treat_all
      ))
    }
  }
  return(results)
}


# Run for all files
all_net_benefits <- rbindlist(lapply(seq_along(file_list), function(i) {
  cat("\nProcessing file:", i, "-", file_list[i], "\n")
  df <- fread(file_list[i])
  calc_net_benefit(df, dataset_params[i])
}))


# For each dataset and model, compute share of centers with beneficial NB
# First, check per center if both conditions are met
center_eval <- all_net_benefits[
  , .(NB = mean(NB), NB_treat_all = mean(NB_treat_all)), 
  by = .(Dataset, Model, center)
][
  , .(
    beneficial = NB > 0 & NB > NB_treat_all
  ), 
  by = .(Dataset, Model, center)
]

# Then compute share of beneficial centers per model
share_beneficial_nb <- center_eval[
  , .(
    ShareBeneficial = mean(beneficial)
  ),
  by = .(Dataset, Model)
]


# Plot boxplots of share for each dataset
ggplot(share_beneficial_nb, aes(x = Dataset, y = ShareBeneficial)) +
  geom_boxplot() +
  labs(title = "Share of centers with Net Benefit > 0 and > treat all",
       y = "Share",
       x = "Model/Dataset") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
