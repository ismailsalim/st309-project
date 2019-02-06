library(caret)
library(ROCR)
### LOGISTIC REGRESSION ###

### Predicting maths and portuguese grade levels (aggregated G3) without G1 and without G2
## MATH
# train-test split
train <- createDataPartition(df_maths_gl$pass, p=0.7, list=FALSE)
training <- df_maths_gl[train, ]
testing <- df_maths_gl[-train, ]
# fit model 
glm_maths <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
summary(glm_maths)
# test
pred_maths = predict(glm_maths, newdata=testing)
conf_table_maths <- table(pred_maths, testing$pass)
accuracy_maths <-  (sum(diag(conf_table_maths)/sum(conf_table_maths)))
# compute AUC
prob <- predict(glm_maths, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_maths <- performance(pred, measure = "tpr", x.measure = "fpr")
auc_maths <- performance(pred, measure = "auc")
auc_maths <- auc_maths@y.values[[1]]
auc_maths
# Plots
jpeg("images/log_pef_maths.jpg")
plot(performance_maths, main = "Maths")
dev.off

# Cross-validation accuracy
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model <- train(factor(pass) ~ .,  data=df_maths_gl, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred <-  predict(model, newdata=testing)
conf_table_maths_cv <- table(pred, testing$pass)
accuracy_maths_cv <- (sum(diag(conf_table_maths_cv))/sum(conf_table_maths_cv))

## PORTUGUESE
# train-test split
train <- createDataPartition(df_por_gl$pass, p=0.7, list=FALSE)
training <- df_por_gl[train, ]
testing <- df_por_gl[-train, ]
# fit model 
glm_por <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
# test
pred = predict(glm_por, newdata=testing)
conf_table_por <- table(pred, testing$pass)
accuracy_por <- sum(diag(conf_table_por))/sum(conf_table_por)
# results
varImp(glm_por)
# ompute AUC
prob <- predict(glm_por, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_por <- performance(pred, measure = "tpr", x.measure = "fpr")
auc_por <- performance(pred, measure = "auc")
auc_por <- auc_por@y.values[[1]]
# Plots
plot(performance_por)

# Cross-validation accruacy
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model <- train(factor(pass) ~ .,  data=df_por_gl, method="glm", family="binomial",
               trControl = ctrl, tuneLength = 5)
pred = predict(model, newdata=testing)
conf_table_por_cv <- table(pred, testing$pass)
accuracy_cv <- (sum(diag(conf_table_por_cv))/sum(conf_table_por_cv))


### Reducing number of variables to significant variables (goout, absences, and failures)
## MATHS
# varImp(glm_maths) shows the important variables or summary(glm_maths)
glm_maths_reduced <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
# test
pred_maths_reduced = predict(glm_maths_reduced, newdata=testing)
conf_table_maths_reduced <- table(pred_maths_reduced, testing$pass)
accuracy_reduced <- sum(diag(conf_matrix_reduced))/sum(conf_matrix_reduced)
# compute AUC
prob <- predict(conf_table_maths_reduced, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_maths_reduced <- performance(pred, measure = "tpr", x.measure = "fpr")
auc_maths_reduced <- performance(pred, measure = "auc")
auc_maths_reduced <- auc_maths_reduced@y.values[[1]]
# Plots
plot(performance_maths_reduced)

## PORTUGUESE
glm_por_reduced <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
# test
pred = predict(glm_por_reduced, newdata=testing)
conf_table_por_reduced <- table(pred, testing$pass)
accuracy_reduced <- sum(diag(conf_table_por_reduced)/sum(conf_table_por_reduced))
# compute AUC
prob <- predict(glm_por_reduced, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_por_reduced <- performance(pred, measure = "tpr", x.measure = "fpr")
auc_por_reduced <- performance(pred, measure = "auc")
auc_por_reduced <- auc@y.values[[1]]
# Plots
plot(performance_por_reduced)
