library(rpart)				  
library(rpart.plot)			
library(caret)
library(dplyr)
library(plyr)
library(gridExtra)
library(ggplot2)
### DECISION TREES ###

### Predicting Pass/Fail without G1 and without G2 ###
## MATHS ##
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl),nrow(df_maths_gl)*0.70)
test_data <- df_maths_gl[-train_indices,]
# Fitting the base model
base_model <- rpart(pass ~ ., data=df_maths_gl, subset=train_indices, method="class")
# Finding optimal complexity parameter
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(base_model, test_data, type = "class")
conf_matrix_maths <- table(predictions, test_data$pass)
base_accuracy_maths <- mean(predictions == test_data$pass)
# Plots
# Figure 3.1
jpeg("images/base_plot_maths.jpg", width=500, height=600)
rpart.plot(base_model, main="Maths", yesno=2)
dev.off()
# Figure 3.2
jpeg("images/conf_matrix_maths.jpg", width=500, height=600)
fourfoldplot(conf_matrix_maths, main = "Maths")
dev.off()
# Figure 3.3
jpeg("images/cp_plot_maths.jpg", width=500, height=600)
plotcp(base_model, sub="Maths", cex.lab = 1)
dev.off()


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
cv_accuracy_maths <- 1-mean(errors)

# Pruning the base tree
model_pruned <- prune(base_model, cp = optimal_cp)
predictions_pruned <- predict(model_pruned, test_data, type = "class")
conf_matrix_pruned_maths <- table(predictions_pruned, test_data$pass)
accuracy_pruned_maths <- mean(Predicted== test_data$pass)
# PLOTS
jpeg("images/conf_matrix_pruned_maths.jpg", width=500, height=600)
fourfoldplot(conf_matrix_pruned_maths, main = "Maths")
dev.off()

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
cv_accuracy_pruned_maths <- 1-mean(pruned_errors)

## PORTUGUESE ##
# Train/test split
train_indices <- sample(1:nrow(df_por_gl),nrow(df_por_gl)*0.70)
test_data <- df_por_gl[-train_indices,]
# Fitting base model
base_model <- rpart(pass ~ ., data=df_por_gl, subset=train_indices, method="class")
# finding optimal
optimal_cp <- data.frame(base_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <- predict(base_model, test_data, type = "class")
conf_matrix_por <- table(predictions, test_data$pass)
base_accuracy_por <- mean(predictions == test_data$pass)
# Plots
jpeg("images/base_plot_por.jpg", width=500, height=600)
rpart.plot(base_model, main="Portuguese", yesno=2)
dev.off()
jpeg("images/conf_matrix_por.jpg", width=500, height=600)
fourfoldplot(conf_matrix_por, main = "Portuguese")
dev.off()
jpeg("images/cp_plot_por.jpg", width=500, height=600)
cp_plot_por <- plotcp(base_model, cex.lab=1, sub="Portuguese") 
dev.off()

# Checking the performance of base model using cross-validation
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
cv_accuracy_por <- 1-mean(errors)

# Pruning the base tree
model_pruned <- prune(base_model, cp = optimal_cp)
predictions_pruned <- predict(model_pruned, test_data, type = "class")
conf_matrix_pruned_por <- table(predictions_pruned, test_data$pass)
accuracy_pruned_por <- mean(predictions_pruned == test_data$pass)
# PLOTS
jpeg("images/conf_matrix_pruned_por.jpg", width=500, height=600)
fourfoldplot(conf_matrix_pruned_por, main = "Portuguese")
dev.off()

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
cv_accuracy_pruned_por <-1-mean(pruned_errors)

### Using the balanced dataframe instead ###
## Maths
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl_balanced),nrow(df_maths_gl_balanced)*0.70)
test_data <- df_maths_gl_balanced[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_maths_gl_balanced, subset=train_indices, method="class")
# Finding optimal cp
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
conf_matrix_balanced_maths <- table(predictions, test_data$pass)
balanced_accuracy_maths <- (sum(diag(conf_matrix_balanced))/sum(conf_matrix_balanced))
# Figures
jpeg("images/base_plot_maths_balanced.jpg", width=500, height=600)
rpart.plot(balanced_model, main="Maths (Balanced)", yesno=2)
dev.off()
jpeg("images/cp_plot_maths_balanced.jpg", width=500, height=600)
cp_plot_por <- plotcp(balanced_model, cex.lab=1, sub="Maths (Balanced)") 
dev.off()
jpeg("images/conf_matrix_maths_balanced.jpg", width=500, height=600)
fourfoldplot(conf_matrix_balanced_maths, main = "Maths (Balanced)")
dev.off()
# Pruning the balanced tree
model_pruned <- prune(balanced_model, cp = optimal_cp)
predictions_pruned <- predict(model_pruned, test_data, type = "class")
conf_matrix_pruned_maths <- table(predictions_pruned, test_data$pass)
accuracy_pruned_maths <- mean(predictions_pruned == test_data$pass)
# PLOTS
jpeg("images/conf_matrix_pruned_maths_balanced.jpg", width=500, height=600)
fourfoldplot(conf_matrix_pruned_maths, main = "Maths")
dev.off()


## Portuguese
# Train/test split
train_indices <- sample(1:nrow(df_por_gl_balanced),nrow(df_por_gl_balanced)*0.70)
test_data <- df_por_gl_balanced[-train_indices,]
# Fitting base model
balanced_model <- rpart(pass ~ ., data=df_por_gl_balanced, subset=train_indices, method="class")
# Finding optimal cp
optimal_cp <- data.frame(balanced_model$cptable) %>% arrange(desc(xerror)) %>% select(CP) %>% top_n(1)
optimal_cp <- optimal_cp[[1]]
# Evaluate hold-out accuracy
predictions <-predict(balanced_model, test_data, type = "class")
conf_matrix_balanced_por <- table(predictions, test_data$pass)
balanced_accuracy_por <- mean(predictions == test$pass)
## Figures
jpeg("images/decisiontree/base_plot_por_balanced.jpg", width=500, height=600)
rpart.plot(balanced_model, main="Portuguese (Balanced)", yesno=2)
dev.off()
jpeg("images/decisiontree/cp_plot_por_balanced.jpg", width=500, height=600)
cp_plot_por <- plotcp(balanced_model, cex.lab=1, sub="Portuguese (Balanced)") 
dev.off()
jpeg("images/decisiontree/conf_matrix_por_balanced.jpg", width=500, height=600)
fourfoldplot(conf_matrix_balanced_por, main = "Portuguese (Balanced)")
dev.off()
# Pruning the balanced tree
model_pruned <- prune(balanced_model, cp = optimal_cp)
predictions_pruned <- predict(model_pruned, test_data, type = "class")
conf_matrix_pruned_por <- table(predictions_pruned, test_data$pass)
accuracy_pruned_por <- mean(predictions_pruned == test_data$pass)
# PLOTS
jpeg("images/decisiontree/conf_matrix_pruned_por_balanced.jpg", width=500, height=600)
fourfoldplot(conf_matrix_pruned_maths, main = "Portuguese")
dev.off()

