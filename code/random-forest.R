library(randomForest)
library(mlbench)
library(caret)
library(e1071)

# trainin control parameters
control <- trainControl(method='repeatedcv', number=10, repeats=3)

## Predicting maths grade level (aggregated G3) without G1 and without G2 
mtry <- sqrt(ncol(df_maths_gl[,-1])) # set mtry = square root of number of columns 
tunegrid <- expand.grid(.mtry=mtry)
rf_base_maths <- train(factor(pass)~., 
                    data=df_maths_gl, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_base_maths)
varImp(rf_base_maths)
# Using random search to choose 15 random values for mtry
rf_random_maths <- train(factor(pass) ~ .,
                   data = df_maths_gl,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random_maths)
var_imp_maths <- varImp(rf_random_maths)
# Figures
mtry_plot_maths <- plot(rf_random_maths)

# Predicting portuguese grade level (aggregated G3) without G1 and without G2 
mtry <- sqrt(ncol(df_por_gl[,-1])) # set mtry = square root of number of features 
tunegrid <- expand.grid(.mtry=mtry)
rf_base_por <- train(factor(pass)~., 
                 data=df_por_gl, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_base_por)
var_imp_por <- varImp(rf_base_por)
# Using random search to choose 15 random values for mtry
rf_random_por <- train(factor(pass) ~ .,
                   data = df_por_gl,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random_por)
varImp(rf_random_por)
# Figures
mtry_plot_por <- plot(rf_random_por)

### Predicting maths grade level with G1
mtry <- sqrt(ncol(df_maths_gl2[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_base <- train(factor(pass)~., 
                 data=df_maths_gl2, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_base)
varImp(rf_base)
# Using random search to choose 15 random values for mtry
rf_random_maths2 <- train(factor(pass) ~ .,
                   data = df_maths_gl2,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random_maths2)
var_imp_maths2 <- varImp(rf_random_maths2)
# Figures
mtry_plot_maths2 <- plot(rf_random_maths2)

### Predicting portuguese grade level with G1
mtry <- sqrt(ncol(df_por_gl2[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_base <- train(factor(pass)~., 
                 data=df_por_gl2, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_base)
varImp(rf_base)
# Using random search to choose 15 random values for mtry
rf_random_por2 <- train(factor(pass) ~ .,
                          data = df_por_gl2,
                          method = 'rf',
                          metric = 'Accuracy',
                          tuneLength  = 15, 
                          trControl = control)
print(rf_random_por2)
var_imp_por2 <- varImp(rf_random_por2)
# Figures
mtry_plot_por2 <- plot(rf_random_por2)



### Predicting maths grade level with G1 and G2
mtry <- sqrt(ncol(df_maths_gl3[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_maths <- train(factor(pass)~., 
                 data=df_maths_gl3, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_maths)
varImp(rf_maths)
# Using random search to choose 15 random values for mtry
rf_random_maths3 <- train(factor(pass) ~ .,
                   data = df_maths_gl3,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random_maths3)
var_imp_maths3 <- varImp(rf_random_maths3)
# Figures
mtry_plot_maths3 <- plot(rf_random_maths3)
### Predicting portguese grade level with G1 and G2
mtry <- sqrt(ncol(df_por_gl3[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_por <- train(factor(pass)~., 
                 data=df_por_gl3, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_por)
varImp(rf_por)
# Using random search to choose 15 random values for mtry
rf_random_por3 <- train(factor(pass) ~ .,
                          data = df_por_gl3,
                          method = 'rf',
                          metric = 'Accuracy',
                          tuneLength  = 15, 
                          trControl = control)
print(rf_random_por3)
var_imp_por3 <- varImp(rf_random_por3)
# Figures
mtry_plot_por3 <- plot(rf_random_por3)



### Predicting erasmus grade categories for maths
mtry <- sqrt(ncol(df_maths_cat[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_maths2 <- train(factor(category)~., 
                 data=df_maths_cat, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_maths2)
varImp(rf_maths2)
# # Using random search to choose 15 random values for mtry
rf_maths_cat <- train(factor(category) ~ .,
                        data = df_maths_cat,
                        method = 'rf',
                        metric = 'Accuracy',
                        tuneLength  = 15, 
                        trControl = control)
print(rf_maths_cat)
var_imp_maths_cat <- varImp(rf_maths_cat)
# Figures
mtry_plot_maths_cat<- plot(rf_maths_cat)

### Predicting erasmus grade categories for portuguese
mtry <- sqrt(ncol(df_por_cat[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_por2 <- train(factor(category)~., 
                 data=df_por_cat, 
                 method='rf', 
                 metric='Accuracy', 
                 tuneGrid=tunegrid, 
                 trControl=control)
print(rf_por2)
varImp(rf_por2)
# Using random search to choose 15 random values for mtry
rf_por_cat <- train(factor(category) ~ .,
                      data = df_por_cat,
                      method = 'rf',
                      metric = 'Accuracy',
                      tuneLength  = 15, 
                      trControl = control)
print(rf_por_cat)
var_imp_por_cat <- varImp(rf_por_cat)
# Figures
mtry_plot_por_cat<- plot(rf_por_cat)

