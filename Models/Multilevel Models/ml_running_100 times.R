library(pROC)
library(caret)
library(dplyr)
library(rms)
library(lme4)
library(glmmTMB)



model_5_5_100<- vector("list", 100)
train_list <- vector("list", 100)
test_list<-vector("list",100)
ml_list <- vector("list", 100)

results_list <- vector("list", 100)
results_in_sample <- vector("list", 100)
results_out_sample <- vector("list", 100)
rm(auc_values,o_e_ratios,net_benefits,results_in_sample,results_out_sample)

# Initialize numeric vectors for storing evaluation metrics
auc_values <- numeric(100)
o_e_ratios <- numeric(100)
net_benefits <- numeric(100)

auc_values_in_sample <- numeric(100)
o_e_ratios_in_sample <- numeric(100)
net_benefits_in_sample <- numeric(100)

auc_values_out_sample <- numeric(100)
o_e_ratios_out_sample <- numeric(100)
net_benefits_out_sample <- numeric(100)

# Binary and ordinal variable definitions
binary_vars <- c("bilateral", "loc10", "Ascites", "papflow", "oncocenter") # for center type
binary_vars <- c("bilateral", "loc10", "Ascites", "papflow") # for prevalence

ordinal_vars <- list(
  colscore = c(1, 2, 3, 4),
  Echogenicity = c(1, 2, 3, 4, 5, 6)
)

test_prob_list <- vector("list", 100)


for (i in 1:100) {
  
  # Generate training dataset
  model_5_5_100[[i]] <- generate_train_set()
  train_list[[i]] <- as.data.frame(model_5_5_100[[i]]$train_set)  
  test_list[[i]]  <- as.data.frame(model_5_5_100[[i]]$test_set) 
  
  dd <- datadist(train_list[[i]])
  options(datadist = "dd")
  
  # Convert binary and ordinal variables
  train_list[[i]][binary_vars] <- lapply(train_list[[i]][binary_vars], factor)
  for (var in names(ordinal_vars)) {
    train_list[[i]][[var]] <- factor(train_list[[i]][[var]], ordered = TRUE, levels = ordinal_vars[[var]])
  }
  test_list[[i]][binary_vars] <- lapply(test_list[[i]][binary_vars], factor)
  for (var in names(ordinal_vars)) {
    test_list[[i]][[var]] <- factor(test_list[[i]][[var]], ordered = TRUE, levels = ordinal_vars[[var]])
  }
  
  #fit rcs model
  formula<-outcome1 ~ rcs(Age, 3) + rcs(lCA125,3)  + 
    bilateral + loc10 + Ascites + papflow + colscore + oncocenter + (1 | center) #### change to prevalence when apply to prevalence
  
  # Fit logistic regression model
  ml_list[[i]] <- glmmTMB(formula, data = train_list[[i]],
                 family = binomial(link = "logit"))
  #ml_list[[i]] <- glmer(formula, data = train_list[[i]],family = binomial(link = "logit"),
                        #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))) # try if not converging
  test_prob_list[[i]] <- data.frame(
    prob <- predict(ml_list[[i]], test_list[[i]], allow.new.levels = TRUE, type = "response"),
    in_sample = test_list[[i]]$center %in% train_list[[i]]$center
  )
  # Print information for each model
  cat("Processed model", i, "\n")
}

test_prob_df <- as.data.frame(do.call(cbind, test_prob_list))
n_cols <- ncol(test_prob_df)
new_colnames <- unlist(lapply(seq(1, n_cols, by = 2), function(i) {
  idx <- (i + 1) / 2
  c(paste0("model_", idx), paste0("in_sample_", idx))
}))

colnames(test_prob_df) <- new_colnames


# Rename columns to indicate the model number
#colnames(test_prob_df) <- paste0("Model_", 1:100)
test_prob_df$outcome1<-test_data$outcome1
test_prob_df$center<-test_data$center
test_prob_df$oncocenter<-test_data$oncocenter
test_prob_df
warnings()

# View the first few rows
write.csv(test_prob_df, "~/master_thesis/Multilevel/mls/probs/test_prob_8_2_1000_small_prevalence.csv.gz", row.names = FALSE)


df1<-read.csv("//stud-home.icts.kuleuven.be/r0920443/Desktop/LR/5_5_100_nocentre/test_prob_5_5_100_nocentre.csv.gz")
df1

results_df <- data.frame(
  AUC = auc_values,
  O_E_Ratio = o_e_ratios,
  Net_Benefit = net_benefits,
  AUC_In_Sample = auc_values_in_sample,
  O_E_Ratio_In_Sample = o_e_ratios_in_sample,
  Net_Benefit_In_Sample = net_benefits_in_sample,
  AUC_Out_Sample = auc_values_out_sample,
  O_E_Ratio_Out_Sample = o_e_ratios_out_sample,
  Net_Benefit_Out_Sample = net_benefits_out_sample
)

write.csv(results_df, "//stud-home.icts.kuleuven.be/r0920443/Desktop/LR/5_5_100_nocentre/restuls_df_5_5_100_nocentre.csv", row.names = FALSE)

results_s <- data.frame(
  AUC = auc_values,
  O_E_Ratio = o_e_ratios,
  Net_Benefit = net_benefits
)

write.csv(results_s, "//stud-home.icts.kuleuven.be/r0920443/Desktop/LR/5_5_100_nocentre/restuls_s_5_5_100_nocentre.csv", row.names = FALSE)


group_metrics<- list()
test_result <- list()

for (i in 1:100) {
  # Extract the relevant columns for the current model
  prob_col <- paste0("model_", i)
  in_sample_col <- paste0("in_sample_", i)
  
  # Combine predictions with the center and outcome data from the corresponding test list
  test_result[[i]] <- data.frame(
    center = test_list[[i]]$center,
    outcome = as.numeric(test_list[[i]]$outcome1),
    prob = as.numeric(test_prob_df[[prob_col]]),       # Explicitly convert to numeric
    pred = ifelse(as.numeric(test_prob_df[[prob_col]])>0.5,1,0), # Explicitly convert to numeric
    in_sample = test_prob_df[[in_sample_col]]
  )
  
  # Calculate metrics grouped by center
  group_metrics[[i]] <- test_result[[i]] %>%
    group_by(center,in_sample) %>%
    summarise(
      AUC = as.numeric(pROC::auc(outcome,prob)),  # Ensure numeric input
      TP = sum(outcome==1&pred==1),
      FP = sum(outcome==0&pred==1),
      TN = sum(outcome==0&pred==0),
      FN = sum(outcome==1&pred==0),
      pos = sum(outcome),
      expect = sum (pred),
      expect_prob = sum(prob),
      .groups = "drop"
    ) %>%
    mutate(model = paste0("model_", i))  # Add model identifier
}


# Combine all group metrics into one data frame
group_metrics_df <- do.call(rbind, group_metrics)
group_metrics_df
mean(group_metrics_df$AUC)

write.csv(group_metrics_df, "~/master_thesis/Multilevel/mls/metrics/group_metrics_8_2_1000_small_prevalence.csv", row.names = FALSE)

df2<-read.csv("~/master_thesis/logistic regression/test/metrics/group_metrics_8_2_500_nocenter.csv")


# Compute mean results for each model type

# Compute AUC confidence intervals
# Compute standard error of AUC
auc_se <- sd(results_df$AUC, na.rm = TRUE) / sqrt(sum(!is.na(results_df$AUC)))
auc_mean <- mean(results_df$AUC, na.rm = TRUE)

# Compute 95% confidence interval
auc_ci_lower <- auc_mean - (1.96 * auc_se)
auc_ci_upper <- auc_mean + (1.96 * auc_se)

# Compute mean for other metrics
o_e_mean <- mean(results_df$O_E_Ratio, na.rm = TRUE)
net_benefit_mean <- mean(results_df$Net_Benefit, na.rm = TRUE)

# Create final results dataframe
final_results <- data.frame(
  AUC = auc_mean,
  #AUC_CI_Lower = auc_ci_lower,
  #AUC_CI_Upper = auc_ci_upper,
  O_E_Ratio = o_e_mean,
  Net_Benefit = net_benefit_mean
)

# Print final results
print(final_results)

save(lr_list, train_list, test_list, file = "//stud-home.icts.kuleuven.be/r0920443/Desktop/LR/5_5_100_nocentre/all_models_5_5_100_nocentre.RData")
load("//stud-home.icts.kuleuven.be/r0920443/Desktop/LR/5_5_100_nocentre/all_models_5_5_100_nocentre.RData")# Save


