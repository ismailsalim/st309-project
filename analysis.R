### READING DATA ###
df_por <- read.csv(file="data/student-por.csv", sep=";")
df_maths <- read.csv(file="data/student-mat.csv", sep=";")

### DATA EXPLORATION ###

# dimensions of tables
dim(df_maths) # 395 x 33
dim(df_por) # 649 x 33

# find any missing values
sapply(df_por, function(x) sum(is.na(x))) # there are no missing values
sapply(df_maths, function(x) sum(is.na(x))) # there are no missing values

# information for table that describes variables
str(df_maths)
str(df_por)
# add information about the variables

library(dplyr)
# dividing G3 into two groups (G3 > 10) and removing G1,G2, and G3
df_maths_gl <- df_maths %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)
df_por_gl <- df_por %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)

library(ggplot2)
# plotting distributions ofvariabels across pass and fail groups
ggplot(df_maths_gl, aes(x=sex, fill=pass)) + geom_bar(aes(y=(..count..)/sum(..count..))) + 
  ggtitle("Distribution of Sex") +
  theme(axis.text.x=element_blank()) + theme_minimal()

## Distibutions of Variables Across Maths and Portuguese Datasets ##
# saving number of rows in dataframes
maths_rows <- nrow(df_maths_gl)
por_rows <- nrow(df_por_gl)

# iterate over columns of maths dataframe and assign variables to individual column "distributions"
for(i in 1:ncol(df_maths_gl)){
  nam <- paste0(colnames(df_maths_gl)[i],"_dist_maths")
  if(class(df_maths_gl[,i])=='factor'){
    # if factor list
    assign(nam, table(df_maths_gl[,i])/maths_rows)
  }
  else if(class(df_maths_gl[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_maths_gl[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_maths_gl[,i])/maths_rows)
  }
}

# repeat for portugues dataframe
for(i in 1:ncol(df_por_gl)){
  nam <- paste0(colnames(df_por_gl)[i],"_dist_por")
  if(class(df_por_gl[,i])=='factor'){
    # if factor list
    assign(nam, table(df_por_gl[,i])/por_rows)
  }
  else if(class(df_por_gl[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_por_gl[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_por_gl[,i])/por_rows)
  }
}

