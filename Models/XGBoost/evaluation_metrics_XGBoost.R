#### evaluationmetrics for XGBoost ####
#### script for calculating the evaluation metrics ####
library(pROC)

# Function to evaluate the model
evaluation_metrics <- function(outcome1, predictions, probs) {
  
  # AUC
  auc_result <- roc(outcome1, probs)
  auc_value <- auc(auc_result)
  
  # OE-ratio
  observed <- sum(outcome1)
  expected <- sum(predictions)
  oe_ratio <- observed / expected
  
  # net benefit 
  t = 0.1
  N <- length(outcome1)
  pred_pos <- probs >= t
  TP <- sum(pred_pos & (outcome1 == 1))
  FP <- sum(pred_pos & (outcome1 == 0))
  net_benefit_value <- (TP/N) - (FP/N) * (t/(1-t))
  
  # flexible calibration curves
  
  return(c(AUC = auc_value, O_E = oe_ratio, net_benefit = net_benefit_value))
}