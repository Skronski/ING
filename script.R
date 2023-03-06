# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)



path <- 'D:\\Informatyka i ekonometria\\ING'
setwd(path)



# reading data ------------------------------------------------------------

df <- read.csv('credit_sample.csv')

summary(df)
df$obs_date <-  as.Date(df$obs_date, format = "%Y-%m-%d")



# visualization -----------------------------------------------------------


df %>% ggplot(aes(x = df$default)) +
  geom_bar()+
  xlab('default')




df_default <- df[df$default==1,]
df_no_default <- df[df$default==0,]


#test
