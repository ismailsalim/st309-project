## Data pre-processing ##
library(dplyr)
df_por <- read.csv(file="data/student-por.csv", sep=";")
df_maths <- read.csv(file="data/student-mat.csv", sep=";")
df_maths_gl <- df_maths %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)
df_por_gl <- df_por %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)

# Balancing the data frames
# Indices of Maths passers
pass_indices <- which(df_maths_gl$pass == T)
fail_indices <- which(df_maths_gl$pass == F)
# Number of fails
failed_number <- length(which(df_maths_gl$pass == F))
# Sample and balance
pass_sampled <- sample(pass_indices, failed_number)
df_maths_gl_balanced <- df_maths_gl[c(pass_sampled,fail_indices),]

# Indices of Portuguese passers
pass_indices <- which(df_por_gl$pass == T)
fail_indices <- which(df_por_gl$pass == F)
# Number of fails
failed_number <- length(which(df_por_gl$pass == F))
# Sample and balance
pass_sampled <- sample(pass_indices, failed_number)
df_por_gl_balanced <- df_por_gl[c(pass_sampled,fail_indices),]

