library(randomForest)
library(ggplot2)
## Predicting maths grade level (aggregated G3) without G1 and without G2 ##
# Train/test split
train_indices <- sample(1:nrow(df_maths_gl),nrow(df_maths_gl)*0.70)
# Fitting random forest
rf <- randomForest(factor(pass) ~ . , data = df_maths_gl , subset = train_indices)
class(rf$confusion)
# Figures
plot(rf)
ggplot(data =  data.frame(rf$confusion), mapping = aes(x = "FALSE", y = "TRUE")) +
  geom_tile(aes(fill = "FALSE"), colour = "white")


+
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(2816, 248, 34, 235)
df <- data.frame(TClass, PClass, Y)
df
