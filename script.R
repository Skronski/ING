# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)


path <- 'D:\\Informatyka i ekonometria\\ING'
setwd(path)



# reading data ------------------------------------------------------------

df <- read.csv('credit_sample.csv')

summary(df)
df$obs_date <-  as.Date(df$obs_date, format = "%Y-%m-%d")



# missing data ------------------------------------------------------------

stat <- summary(df)

nas <- as.data.frame(stat[7,])

df_without_na <- na.omit(df)
df_without_na_default <- df_without_na[df_without_na$default==1,]



# visualization -----------------------------------------------------------
df_viz <- df[,!names(df) %in% c('ID')]




df_viz %>% ggplot(aes(x = df_viz$default)) +
  geom_bar()+
  xlab('default')


df_viz %>% ggplot(aes(x = Var_07, y = default))+
  geom_jitter()








# splitting data  ---------------------------------------------------------



df_default <- df[df$default==1,]
df_no_default <- df[df$default==0,]


#test
