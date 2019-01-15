library(randomForest)
library(mlbench)
library(caret)
library(e1071)
## Predicting maths grade level (aggregated G3) without G1 and without G2 ##
# 10 folds repeated 3 times
control <- trainControl(method='repeatedcv', number=10, repeats=3)
# Set mtry = square root of number of columns 
mtry <- sqrt(ncol(df_maths_gl[,-1]))
tunegrid <- expand.grid(.mtry=mtry)
rf_base <- train(factor(pass)~., 
                    data=df_maths_gl_balanced, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_base)

# Using random search to choose 15 random values for mtry
rf_random <- train(factor(pass) ~ .,
                   data = df_maths_gl_balanced,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random)
# Figures
plot(rf_random)


