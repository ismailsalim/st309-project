library(rpart)				  
library(rattle)				
library(rpart.plot)			
library(RColorBrewer)				
library(party)					
library(partykit)				
library(caret)
library(plyr)

###########
###MATHS###
###########
## Predicting maths grade level (aggregated G3) without G1 and without G2 ##
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl),nrow(df_maths_gl)*0.70)
test_data <- df_maths_gl[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_maths_gl, subset=train_indices, method="class")
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
paste("Base accuracy is:", round(base_accuracy, 3))
conf_matrix
# PLOT
fancyRpartPlot(base_model)
plotcp(base_model) 

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
# rpart is unable to create a decision tree with the data  

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
conf_matrix_balanced <- table(predictions, test_data$pass)
balanced_accuracy <- (sum(diag(conf_matrix_balanced))/sum(conf_matrix_balanced))
conf_matrix_balanced
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))



##############################################################################################
# Adding G1
# Using dataset df_maths_gl2

# Train/test split
train_indices <- sample(1:nrow(df_maths_gl2),nrow(df_maths_gl2)*0.70)
test_data <- df_maths_gl2[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_maths_gl2, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
conf_matrix
paste("Base accuracy is:", round(base_accuracy, 3))


# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl2, cut(sample(1:nrow(df_maths_gl2)),10))
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
# Again, unable to prune the tree

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl2, cut(sample(1:nrow(df_maths_gl2)),10))
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
train_indices <- sample(1:nrow(df_maths_gl_balanced2),nrow(df_maths_gl_balanced2)*0.70)
test_data <- df_maths_gl_balanced2[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_maths_gl_balanced2, subset=train_indices2, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
conf_matrix_balanced <- table(predictions, test_data$pass)
balanced_accuracy <- (sum(diag(conf_matrix_balanced))/sum(conf_matrix_balanced))
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))

##############################################################################################
# Adding G1 and G2
# Use dataset df_maths_gl3

# Train/test split
train_indices <- sample(1:nrow(df_maths_gl3),nrow(df_maths_gl3)*0.70)
test_data <- df_maths_gl3[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_maths_gl3, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
conf_matrix
paste("Base accuracy is:", round(base_accuracy, 3))

# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl3, cut(sample(1:nrow(df_maths_gl3)),10))
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
plot(model_pruned)
text(model_pruned, pretty=1, cex=0.5)

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_maths_gl3, cut(sample(1:nrow(df_maths_gl3)),10))
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
train_indices <- sample(1:nrow(df_maths_gl_balanced3),nrow(df_maths_gl_balanced3)*0.70)
test_data <- df_maths_gl_balanced3[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_maths_gl_balanced3, subset=train_indices, method="class")
## Figures
plot(balanced_model)
text(balanced_model, pretty=1, cex=0.5)
plotcp(balanced_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
table(predictions, test_data$pass)
balanced_accuracy <- mean(predictions == test$pass)
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))




################
###Portuguese###
################
## Predicting portuguese grade level (aggregated G3) without G1 and without G2 ##
# Train/test split
train_indices <- sample(1:nrow(df_por_gl),nrow(df_por_gl)*0.70)
test_data <- df_por_gl[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_por_gl, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
paste("Base accuracy is:", round(base_accuracy, 3))

# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl, cut(sample(1:nrow(df_por_gl)),10))
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
plot(model_pruned)
text(model_pruned, pretty=1, cex=0.5)

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl, cut(sample(1:nrow(df_por_gl)),10))
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
train_indices <- sample(1:nrow(df_por_gl_balanced),nrow(df_por_gl_balanced)*0.70)
test_data <- df_por_gl_balanced[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_por_gl_balanced, subset=train_indices, method="class")
## Figures
plot(balanced_model)
text(balanced_model, pretty=1, cex=0.5)
plotcp(balanced_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
table(predictions, test_data$pass)
balanced_accuracy <- mean(predictions == test$pass)
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))



##############################################################################################
#predicting the portuguese grade level (G3) without G2. So now we have the extra information of G1
#use dataset df_por_gl2

# Train/test split
train_indices <- sample(1:nrow(df_por_gl2),nrow(df_por_gl2)*0.70)
test_data <- df_por_gl2[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_por_gl2, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
paste("Base accuracy is:", round(base_accuracy, 3))

# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl2, cut(sample(1:nrow(df_por_gl2)),10))
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
plot(model_pruned)
text(model_pruned, pretty=1, cex=0.5)

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl2, cut(sample(1:nrow(df_por_gl2)),10))
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
train_indices <- sample(1:nrow(df_por_gl_balanced2),nrow(df_por_gl_balanced2)*0.70)
test_data <- df_por_gl_balanced2[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_por_gl_balanced2, subset=train_indices, method="class")
## Figures
plot(balanced_model)
text(balanced_model, pretty=1, cex=0.5)
plotcp(balanced_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
table(predictions, test_data$pass)
balanced_accuracy <- mean(predictions == test$pass)
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))


##############################################################################################
#predicting the portuguese grade level (G3). So now we have the extra information of G1 and G2
#use dataset df_por_gl3

# Train/test split
train_indices <- sample(1:nrow(df_por_gl3),nrow(df_por_gl3)*0.70)
test_data <- df_por_gl3[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_por_gl3, subset=train_indices, method="class")
## Figures
fancyRpartPlot(base_model)
plotcp(base_model) 
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$pass)
base_accuracy <- mean(predictions == test_data$pass)
paste("Base accuracy is:", round(base_accuracy, 3))

# Checking the performance of base model (potential bias of simple hold out) using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl3, cut(sample(1:nrow(df_por_gl3)),10))
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
plot(model_pruned)
text(model_pruned, pretty=1, cex=0.5)

# Checking the performance of pruned model using cross-validation
formula <- "pass ~ ."
folds <- split(df_por_gl3, cut(sample(1:nrow(df_por_gl3)),10))
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
train_indices <- sample(1:nrow(df_por_gl_balanced3),nrow(df_por_gl_balanced3)*0.70)
test_data <- df_por_gl_balanced3[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_por_gl_balanced3, subset=train_indices, method="class")
## Figures
plot(balanced_model)
text(balanced_model, pretty=1, cex=0.5)
plotcp(balanced_model) 
# finding optimal
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
table(predictions, test_data$pass)
balanced_accuracy <- mean(predictions == test$pass)
paste("Accuracy using balanced dataset is:", round(balanced_accuracy, 3))

