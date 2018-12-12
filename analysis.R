# Reading Data
df_por <- read.csv(file="data/student-por.csv", sep=";")
df_maths <- read.csv(file="data/student-mat.csv", sep=";")

# DATA EXPLORATION

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

# ANALYSIS

library(dplyr)
df_maths_gl <- df_maths %>% mutate(pass = G3>=10)
df_por_gl <- df_por %>% mutate(pass = G3>=10)


