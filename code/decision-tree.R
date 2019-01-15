library(rpart)				  
library(rattle)				
library(rpart.plot)			
library(RColorBrewer)				
library(party)					
library(partykit)				
library(caret)
library(plyr)
## Predicting maths grade level (aggregated G3) without G1 and without G2 ##
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl),nrow(df_maths_gl)*0.70)
test_data <- df_maths_gl[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_maths_gl, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test$pass)
paste("Base accuracy is:", round(base_accuracy, 3))

# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl, cut(sample(1:nrow(df_maths_gl)),10))
errors <- rep(NA, length(folds))
for(i in 1:length(folds)){
  test <- ldply(folds[i], data.frame)[,-1]
  train <- ldply(folds[-i], data.frame)[,-1]
  temp.model <- rpart(formula=formula, data=train, method="class")
  temp.predict <- predict(temp.model, newdata=test, type="class")
  conf_matrix <- table(test$pass, temp.predict)
  errors[i] <- 1 - (sum(diag(conf_matrix))/sum(conf_matrix))
}
paste("Cross-validation accuracy of base tree is:", round(1-mean(errors), 3))

# Pruning the base tree
model_pruned <- prune(base_model, cp = optimal_cp)
predictions_pruned <- predict(model_pruned, test_data, type = "class")
conf_matrix_pruned <- table(predictions_pruned, test_data$pass)
accuracy_pruned <- mean(predictions_pruned == test_data$pass)
conf_matrix_pruned
paste("Pruned accuracy is", round(accuracy_pruned,3))
# Figures
fancyRpartPlot(model_pruned)

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl, cut(sample(1:nrow(df_maths_gl)),10))
pruned_errors <- rep(NA, length(folds))
for(i in 1:length(folds)){
  test <- ldply(folds[i], data.frame)[,-1]
  train <- ldply(folds[-i], data.frame)[,-1]
  temp_model <- rpart(formula=formula, data=train, method="class")
  
  optimal_cp <- data.frame(temp_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
  optimal_cp <- optimal_cp[[1]]
  
  temp_pruned_model <- prune(temp_model, optimal_cp)
  temp_pruned_predict <- predict(temp_pruned_model, newdata=test, type="class")
  conf_matrix_pruned <- table(test$pass, temp_pruned_predict)
  pruned_errors[i] <- 1 - (sum(diag(conf_matrix_pruned))/sum(conf_matrix_pruned))
}
paste("Cross-validation accuracy of pruned tree is:", round(1-mean(pruned_errors), 3))

# Using the balanced data frame instead
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl_balanced),nrow(df_maths_gl_balanced)*0.70)
test_data <- df_maths_gl_balanced[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_maths_gl_balanced, subset=train_indices, method="class")
## Figures
fancyRpartPlot(balanced_model)
plotcp(balanced_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
table(predictions, test_data$pass)
balanced_accuracy <- mean(predictions == test$pass)
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))

