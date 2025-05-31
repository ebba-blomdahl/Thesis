model1<-glmmTMB(outcome1 ~ rcs(Age, 3) + rcs(lCA125,3)  + 
                  bilateral + loc10 + Ascites + papflow + colscore + oncocenter + (1 | center),
                data = test_data,
                family = binomial(link = "logit"))
model2<-glmmTMB(outcome1 ~ rcs(Age, 3) + rcs(lCA125,3)  + 
                  bilateral + loc10 + Ascites + papflow + colscore  + (1 | center),
                data = test_data,
                family = binomial(link = "logit"))

test_data$pred <- predict(model1, newdata = test_data, type = "response")
ggplot(test_data, aes(x = pred, fill = factor(oncocenter))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Oncocenter") +
  theme_minimal()


summary(model2)
summary(model1)
anova(model1,model2)
library(car)
library(pROC)
roc(test_data$outcome1[test_data$oncocenter == 1],
    test_data$pred[test_data$oncocenter == 1])

roc(test_data$outcome1[test_data$oncocenter == 0],
    test_data$pred[test_data$oncocenter == 0])
