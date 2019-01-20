library(dplyr)

# Reading the csv files
df_maths <- read.csv(file="data/student-mat.csv", sep=";")
df_por <- read.csv(file="data/student-por.csv", sep=";")

# Dataframe without G1, G2, G3
df_maths_gl <- df_maths %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)
df_por_gl <- df_por %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)

# Dataframe without G2, G3 (we now add G1 to our current information)
df_maths_gl2 <- df_maths %>% mutate(pass = G3>=10) %>% select(-G2, -G3) 
df_por_gl2 <- df_por %>% mutate(pass = G3>=10) %>% select(-G2, -G3) 

# Dataframe without G3 (we now add G2 to our current information)
df_maths_gl3 <- df_maths %>% mutate(pass = G3>=10) %>% select(-G3) 
df_por_gl3 <- df_por %>% mutate(pass = G3>=10) %>% select(-G3) 

## Balancing pass/fail proportions of the dataframes without G1 and G2
# MATHS
pass_indices <- which(df_maths_gl$pass == T)
fail_indices <- which(df_maths_gl$pass == F)
# Number of fails
failed_number <- length(which(df_maths_gl$pass == F))
# Sample and balance
pass_sampled <- sample(pass_indices, failed_number)
df_maths_gl_balanced <- df_maths_gl[c(pass_sampled,fail_indices),]

# PORTUGUESE
pass_indices <- which(df_por_gl$pass == T)
fail_indices <- which(df_por_gl$pass == F)
# Number of fails
failed_number <- length(which(df_por_gl$pass == F))
# Sample and balance
pass_sampled <- sample(pass_indices, failed_number)
df_por_gl_balanced <- df_por_gl[c(pass_sampled,fail_indices),]

# G3 as Erasmus grade categories
df_maths_cat <- df_maths %>% 
  mutate(category=cut(G3, breaks=c(-Inf, 9, 11, 13, 15, 20), labels=c("V","IV","III", "II","I"))) %>% select(-G3)
df_por_cat <- df_por %>% 
  mutate(category=cut(G3, breaks=c(-Inf, 9, 11, 13, 15, 20), labels=c("V","IV","III", "II","I"))) %>% select(-G3)

