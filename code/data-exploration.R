### READING DATA ###
df_por <- read.csv(file="data/student-por.csv", sep=";")
df_maths <- read.csv(file="data/student-mat.csv", sep=";")

library(gridExtra)
library(dplyr)
library(ggplot2)

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


# dividing G3 into two groups (G3 > 10) and removing G1,G2, and G3
df_maths_gl <- df_maths %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)
df_por_gl <- df_por %>% mutate(pass = G3>=10) %>% select(-G1, -G2, -G3)


# plotting distributions ofvariabels across pass and fail groups
ggplot(df_maths_gl, aes(x=sex, fill=pass)) + geom_bar(aes(y=(..count..)/sum(..count..))) + 
  ggtitle("Distribution of Sex") +
  theme(axis.text.x=element_blank()) + theme_minimal()

## Distibutions of Variables Across Maths and Portuguese Datasets ##
# saving number of rows in dataframes
maths_rows <- nrow(df_maths_gl)
por_rows <- nrow(df_por_gl)

# iterate over columns of maths dataframe and create variables to individual column summaries/distributions
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

# repeat for portuguese dataframe
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

## Comparing across pass and fail groups ##
# create separate "Pass and Fail" dataframes
df_maths_gl_pass <- df_maths_gl %>% filter(pass == T) %>% select(-pass)
df_maths_gl_fail <- df_maths_gl %>% filter(pass == F) %>% select(-pass)
df_por_gl_pass <- df_por_gl %>% filter(pass == T) %>% select(-pass)
df_por_gl_fail <- df_por_gl %>% filter(pass == F) %>% select(-pass)

maths_pass_rows <- nrow(df_maths_gl_pass)
maths_fail_rows <-  nrow(df_maths_gl_fail)
por_pass_rows <- nrow(df_por_gl_pass)
por_fail_rows <- nrow(df_por_gl_fail)

# creating variables for distributions of maths pass
for(i in 1:ncol(df_maths_gl_pass)){
  nam <- paste0(colnames(df_maths_gl_pass)[i],"_dist_maths_pass")
  if(class(df_maths_gl_pass[,i])=='factor'){
    # if factor list
    assign(nam, table(df_maths_gl_pass[,i])/maths_pass_rows)
  }
  else if(class(df_maths_gl_pass[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_maths_gl_pass[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_maths_gl_pass[,i])/maths_pass_rows)
  }
}

# creating variables for distributions of portuguese pass
for(i in 1:ncol(df_por_gl_pass)){
  nam <- paste0(colnames(df_por_gl_pass)[i],"_dist_por_pass")
  if(class(df_por_gl_pass[,i])=='factor'){
    # if factor list
    assign(nam, table(df_por_gl_pass[,i])/por_pass_rows)
  }
  else if(class(df_por_gl_pass[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_por_gl_pass[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_por_gl_pass[,i])/por_pass_rows)
  }
}

# creating variables for distributions of maths fail
for(i in 1:ncol(df_maths_gl_fail)){
  nam <- paste0(colnames(df_maths_gl_fail)[i],"_dist_maths_fail")
  if(class(df_maths_gl_fail[,i])=='factor'){
    # if factor list
    assign(nam, table(df_maths_gl_fail[,i])/maths_fail_rows)
  }
  else if(class(df_maths_gl_fail[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_maths_gl_fail[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_maths_gl_fail[,i])/maths_fail_rows)
  }
}

# creating variables for distributions of portuguese fail
for(i in 1:ncol(df_por_gl_fail)){
  nam <- paste0(colnames(df_por_gl_fail)[i],"_dist_por_fail")
  if(class(df_por_gl_fail[,i])=='factor'){
    # if factor list
    assign(nam, table(df_por_gl_fail[,i])/por_fail_rows)
  }
  else if(class(df_por_gl_fail[,i])=='integer'){
    # if integer list
    assign(nam, summary(df_por_gl_fail[,i]))
  }
  else{
    # if logical list
    assign(nam, sum(df_por_gl_fail[,i])/por_fail_rows)
  }
}

## Plotting distributions of selected variables ##
# Figure 2.1
maths_grades<- ggplot(df_maths, aes(x=G3)) + geom_histogram(binwidth = 1, fill="#56B4E9") + 
  theme_minimal() + ggtitle("Maths") + ylab("No. of Students") + scale_x_continuous("Final Grades (G3)")
maths_pass<- ggplot(df_maths_gl, aes(x=pass, fill=pass)) + geom_bar() + 
  scale_x_discrete("Student Performance", labels=c("Fail", "Pass")) + 
  theme_minimal() + theme(legend.position = "none", axis.title.y=element_blank())
por_grades <- ggplot(df_por, aes(x=G3)) + geom_histogram(binwidth = 1, fill="#56B4E9") + 
  theme_minimal() + ggtitle("Portuguese") + ylab("No. of Students") + scale_x_continuous("Final Grades (G3)")
por_pass<- ggplot(df_por_gl, aes(x=pass, fill=pass)) + geom_bar() + 
  scale_x_discrete("Student Performance", labels=c("Fail", "Pass")) + 
  theme_minimal() + theme(legend.position = "none", axis.title.y=element_blank())
grid.arrange(maths_grades, maths_pass, por_grades, por_pass, ncol=2)

# Figure 2.2 (absences)
means_absences_maths <- aggregate(absences~pass, df_maths_gl, mean)
means_absences_maths[2] <- round(means_absences_maths[2],3)
means_absences_por <- aggregate(absences~pass, df_por_gl, mean)
means_absences_por[2] <- round(means_absences_por[2],3)
means_goout_maths <- aggregate(goout~pass, df_maths_gl, mean)
means_goout_maths[2] <- round(means_goout_maths[2],3)
means_goout_por <- aggregate(goout~pass, df_por_gl, mean)
means_goout_por[2] <- round(means_goout_por[2],3)
# absences for maths and portuguese
absences_dist_plot_maths <- ggplot(df_maths_gl, aes(x=pass, y=absences, color=pass)) + geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, guide='legend') +
  ggtitle("Maths") + geom_text(data=means_absences_maths, aes(label=absences, y=absences+1)) + 
  scale_x_discrete("Student Performance", labels =c("Fail", "Pass")) + ylim(0,30) +  
  theme_minimal() + theme(legend.position = "none")
absences_dist_plot_por <-  ggplot(df_por_gl, aes(x=pass, y=absences, color=pass)) + geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
  ggtitle("Portuguese") + geom_text(data=means_absences_por, aes(label=absences, y=absences+1)) +
  scale_x_discrete("Student Performance", labels=c("Fail", "Pass")) + ylim(0,20) + 
  theme_minimal() + theme(legend.position = "none")
grid.arrange(absences_dist_plot_por, absences_dist_plot_maths)

#Figure 2.3 (absences)
# goout for maths and portuguese
goout_dist_plot_maths <- ggplot(df_maths_gl, aes(x=pass, y=goout, color=pass)) + geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=5, element_text(size=14)) +
  ggtitle("Maths") + geom_text(data=means_goout_maths, aes(label=goout, y=goout+0.5)) + 
  scale_x_discrete("Student Performance", labels =c("Fail", "Pass"))+ ylim(0,5) + 
  theme_minimal() + theme(legend.position = "none", axis.text=element_text(size=16), axis.title=element_text(size=16), 
                          plot.title=element_text(size=18))
goout_dist_plot_por <-  ggplot(df_por_gl, aes(x=pass, y=goout, color=pass)) + geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=5) +
  ggtitle("Portuguese") + geom_text(data=means_goout_por, aes(label=goout, y=goout+0.5)) + 
  scale_x_discrete("Student Performance", labels =c("Fail", "Pass"))+ ylim(0,5) + 
  theme_minimal() + theme(legend.position = "none", axis.text=element_text(size=16), axis.title=element_text(size=16), 
                          plot.title=element_text(size=18))
# export final plot
jpeg("images/goout_dist_plot.jpg", width=1000, height=600)
grid.arrange(goout_dist_plot_por, goout_dist_plot_maths, ncol=2)
dev.off()

## Correlations between variables for Maths and Portuguese ##
# maths goout vs g3
cor(df_maths$goout, df_maths$G3, method='spearman') # -0.166
cor.test(df_maths$goout, df_maths$G3, method='spearman')
# maths absences vs g3
cor.test(df_maths$absences, df_maths$G3, method='pearson') # cor=0.0342 p=0.4973
# maths goout vs absences
cor(df_maths$goout, df_maths$absences, method="spearman") # 0.133
# portuguese goout vs g3
cor(df_por$goout, df_por$G3, method="spearman") # -0.104
# portuguese absences vs g3
cor(df_por$absences, df_por$G3, method="pearson") # -0.091
# portuguese goout vs absences
cor(df_por$goout, df_por$absences, method="spearman") # 0.104

# plotting correlations of goout and absences
# Figure 2.4 (goout and absences)
goout_absences_plot_maths <- ggplot(df_maths_gl, aes(x=goout, y=absences, color=pass)) + geom_point() + 
  ggtitle("Maths") + labs(subtitle = "Spearman Correlation = 0.133") + theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), plot.title=element_text(size=18),
        plot.subtitle = element_text(size=16), strip.text = element_text(size=14), 
        legend.text=element_text(size=14), legend.title=element_blank())
goout_absences_plot_por <- ggplot(df_por_gl, aes(x=goout, y=absences, color=pass)) + geom_point() + 
  ggtitle("Portuguese") + labs(subtitle="Spearman Correlation = 0.104") + theme_minimal() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), plot.title=element_text(size=18),
        plot.subtitle = element_text(size=16), strip.text = element_text(size=14), 
        legend.text=element_text(size=14), legend.title=element_blank())
# export final plot
jpeg("images/goout_absences_plot.jpg", width=1000, height=500)
grid.arrange(goout_absences_plot_maths, goout_absences_plot_por, ncol=2)
dev.off()

# Figure 2.5 Distribution of schoolsup for failures and passers
# schoolsup yes rates
schoolsup_df <- data.frame(c("Maths", "Maths", "Portuguese", "Portuguese"),
                           c("Pass", "Fail", "Pass", "Fail"),
c(schoolsup_dist_maths_pass[[2]], schoolsup_dist_maths_fail[[2]], schoolsup_dist_por_pass[[2]], schoolsup_dist_por_fail[[2]]))
colnames(schoolsup_df) <- c("subject","grade", "supportrate")
# famsup yes rates
famsup_df <- data.frame(c("Maths", "Maths", "Portuguese", "Portuguese"),
                           c("Pass", "Fail", "Pass", "Fail"),
                           c(famsup_dist_maths_pass[[2]], famsup_dist_maths_fail[[2]], famsup_dist_por_pass[[2]], famsup_dist_por_fail[[2]]))
colnames(famsup_df) <- c("subject","grade", "famsuprate")
# facet wrapped plotl
schoolsup_plot <- ggplot(schoolsup_df, aes(x=grade, y=supportrate, fill=grade)) + geom_bar(stat="identity") + facet_wrap(~subject) + 
  theme_minimal() + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), plot.title=element_text(size=18),
                          strip.text = element_text(size=14), legend.text=element_text(size=14), legend.title=element_blank(), 
                          axis.title.x=element_blank()) + ggtitle("Extra Educational Support")
famsup_plot <- ggplot(famsup_df, aes(x=grade, y=famsuprate, fill=grade)) + geom_bar(stat="identity") + facet_wrap(~subject) + 
  theme_minimal() + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), plot.title=element_text(size=18),
                          strip.text = element_text(size=14), legend.text=element_text(size=14), legend.title=element_blank(), 
                          axis.title.x=element_blank()) + ggtitle("Extra Educational Support")
# export final plot
jpeg("images/famsup_schoolsup_plot.jpg", width=1000, height=600)
grid.arrange(schoolsup_plot, famsup_plot, ncol = 2)
dev.off()

