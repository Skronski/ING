# libraries ---------------------------------------------------------------





path <- 'D:\\Informatyka i ekonometria\\ING'
setwd(path)



# reading data ------------------------------------------------------------

df <- read.csv('credit_sample.csv')

summary(df)
df$obs_date <-  as.Date(df$obs_date, format = "%Y-%m-%d")



# visualization -----------------------------------------------------------







df_default <- df[df$default==1,]
df_no_default <- df[df$default==0,]
