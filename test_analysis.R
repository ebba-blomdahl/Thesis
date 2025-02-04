#### some models for testing #### 

# libraries 
library(randomForest)
library(caret)
library(pROC)
library(xgboost)
library(caret)
library(ROCR)

# Source of the dataset generation function
source("/Users/ebbablomdahl/Thesis/Code/generate_dataset.R")

# import data with specified parameters 
data <- generate_dataset(nbr_oncology = 5, nbr_non_oncology = 5, center_size = 10, center_type_predictor = TRUE)

# split into training and test set (80% vs 20%)
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_set <- data[train_indices, ]  
test_set <- data[-train_indices, ]

# see how balanced the outcome is 
table(train_set$outcome1) # not too imbalanced 

######### Logistic Regression ########

lr_model <- glm(train_set$outcome1 ~.,family=binomial(link='logit'),data=(train_set %>% select(-outcome1)))
summary(lr_model)

anova(lr_model, test="Chisq")

# accuracy 
fitted.results <- predict(lr_model,newdata=(test_set %>% select(-outcome1)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_set$outcome1)
print(paste('Accuracy',1-misClasificError))

# ROC and AUC
lr_prediction <- predict(lr_model, newdata=test_set, type="response")
pr <- prediction(lr_prediction, test_set$outcome1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

######### Random Forest ########

random_forest <- randomForest(as.factor(outcome1)~., data=train_set, proximity=TRUE) # no hyperparameters selected here 
print(random_forest)

# predicting 
predict_rf <- predict(random_forest, test_set, type = "class")

# confusion matrix 
confusionMatrix(as.factor(predict_rf), as.factor(test_set$outcome1))

# ROC 
predict_rf_prob <- predict(random_forest, test_set, type = "prob") # predict probabilities 
roc(test_set$outcome1, predict_rf_prob[,2])
auc(test_set$outcome1, predict_rf_prob[,2])
