evaluation <- function(model, data, outcome_var) {
  # Get predicted probabilities
  predicted_probs <- predict(model, newdata = data, allow.new.levels = TRUE,type = "response")
  
  # Convert to 0/1 based on threshold 0.5
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  
  # Ensure outcome variable is properly extracted
  actual_outcome <- data[[outcome_var]]
  
  # Generate confusion matrix
  conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0,1)), 
                                 factor(actual_outcome, levels = c(0,1)))
  
  # ROC curve and AUC
  roc_curve <- roc(actual_outcome, predicted_probs)
  auc_value <- auc(roc_curve)
  
  # Observed/Expected (O/E) ratio
  O_E_ratio <- sum(actual_outcome) / sum(predicted_classes)
  
  # Net benefit calculation at threshold 0.1
  threshold <- 0.1
  TP <- sum(predicted_classes == 1 & actual_outcome == 1)  # True Positives
  FP <- sum(predicted_classes == 1 & actual_outcome == 0)  # False Positives
  n <- length(actual_outcome)  # Total number of observations
  
  net_benefit <- (TP / n) - ((FP / n) * (threshold / (1 - threshold)))
  
  # Return results as a list
  return(list(
    confusion_matrix = conf_matrix,
    roc_curve = roc_curve,
    auc = auc_value,
    O_E_ratio = O_E_ratio,
    net_benefit = net_benefit
  ))
}
