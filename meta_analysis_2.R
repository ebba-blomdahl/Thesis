library(pROC)
library(dplyr)
library(metamisc)

df<-read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest/results_8_2_1000_FALSE.csv")
# for random forest 
df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)

head(df)

# Function to calculate O/E ratio
calc_oe <- function(pred, outcome) {
  obs <- sum(outcome)
  exp <- sum(pred)
  return(obs / exp)
}

# Function to calculate Net Benefit
calc_nb <- function(pred, outcome, threshold = 0.1) {
  tp <- sum((pred == 1) & (outcome == 1))
  fp <- sum((pred == 1) & (outcome == 0))
  n <- length(outcome)
  return((tp / n) - (fp / n) * (threshold / (1 - threshold)))
}

df2 <- df %>%
  group_by(center) %>% 
  mutate(pos = sum(outcome1 == 1)) %>% 
  distinct(center,pos)

# Prepare a data frame to store the metrics
metrics <- data.frame(Model = character(), center = integer(), AUC = numeric(), O_E_Ratio = numeric(), Net_Benefit = numeric())

# Loop through each iteration
for (idx in 1:100) {
  prob_col <- paste0("probabilities_", idx)
  
  for (c in unique(df$center)) {
    subdata <- subset(df, center == c)
    prob <- subdata[[prob_col]]  # Probability scores
    outcome <- subdata$outcome1
    
    # Calculate metrics
    auc_value <- auc(outcome, prob)  # Think we need to use the predictions and not prob here 
    oe_value <- calc_oe(prob, outcome)  # 
    nb_value <- calc_nb(prob, outcome)  # 
    
    # Store results
    metrics <- rbind(metrics, data.frame(Model = idx, center = c, AUC = auc_value, O_E_Ratio = oe_value, Net_Benefit = nb_value))
    
  }
}

metrics
metrics1<-left_join(metrics,df2,by="center")
metrics1
# Assume sample size per center is available, set to 10000
sample_size <- 10000  

# Initialize list to store meta-analysis results
meta_results <- list()
tau2_values <- list()

# Loop over unique models
for (model in unique(metrics1$Model)) {
  
  # Subset data for the current model
  model_data <- subset(metrics1, Model == model)
  
  # Ensure we have data for the current model
  if (nrow(model_data) > 0) {
    
    # Calculate SE for AUC if needed (assuming ccalc function is available)
    # If you already have SE in est1.theta.se, you might not need this
    model_data$AUC_se <- ccalc(model_data$AUC, N=sample_size, O=model_data$pos)$theta.se
    
    # Run random-effects meta-analysis for AUC using the existing SE values
    meta_auc<- valmeta(measure = "cstat", 
                       cstat = model_data$AUC, 
                       cstat.se = model_data$AUC_se, 
                       method = "REML")
  
    # Store results
    meta_results[[model]] <- meta_auc
    tau2_values[[model]] <- meta_auc$fit$tau2
    
    # Print results
    cat("\nModel:", model, "\n")
    print(meta_auc$fit)
  } else {
    cat("\nSkipping Model:", model, "as it has no data.\n")
  }
}

tau <- as.data.frame(do.call(rbind, tau2_values))

mean(tau$V1)

## to see the AUC plot of model_1
plot(meta_results[[1]])




  


