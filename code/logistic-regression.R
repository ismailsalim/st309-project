library(caret)
library(ROCR)
# MATHS
# Without G1 and G2
# train-test split
train <- createDataPartition(df_maths_gl$pass, p=0.7, list=FALSE)
training <- df_maths_gl[train, ]
testing <- df_maths_gl[-train, ]
# fit model 
base_model <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
summary(base_model)
# test
pred = predict(base_model, newdata=testing)
conf_table <- table(pred, testing$pass)
accuracy <- sum(diag(conf_table))/sum(conf_table)
# results
varImp(base_model)
conf_table
paste("Base model accuracy is", round(accuracy, 3))
# compute AUC
prob <- predict(base_model, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_base <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(performance_base)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Cross-validation accruacy
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model <- train(factor(pass) ~ .,  data=df_maths_gl, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred = predict(model, newdata=testing)
conf_matrix <- table(pred, testing$pass)
accuracy_cv <- (sum(diag(conf_matrix))/sum(conf_matrix))
conf_matrix
accuracy_cv

# Reducing number of variables to significant variables
## goout, absences, and failures
summary(base_model)
reduced_model <- train(factor(pass) ~ .,  data=training, method="glm", family="binomial")
# test
pred_reduced = predict(reduced_model, newdata=testing)
conf_matrix_reduced <- table(pred, testing$pass)
accuracy_reduced <- sum(diag(conf_matrix_reduced))/sum(conf_matrix_reduced)
# results
conf_matrix_reduced
paste("Reduced model accuracy is", round(accuracy_reduced, 3))
# compute AUC
prob <- predict(reduced_model, newdata=testing)
pred <- prediction(as.numeric(prob), as.numeric(testing$pass))
performance_base <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(performance_base)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

