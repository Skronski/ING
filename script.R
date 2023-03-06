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


# outliers, missing values ------------------------------------------------

stat <- summary(df)

nas <- as.data.frame(stat[7, ])


remove_outliers <- function(column)
{
  x <- quantile(column, na.rm = T)
  names(x) <- NULL
  IQR <- x[4] - x[2]
  outlier_flag <- c(x[2] - IQR * 3, x[4] + IQR * 3)
  column <- ifelse(column < outlier_flag[1], x[2], column)
  column <- ifelse(column > outlier_flag[2], x[4], column)
  return(column)
}
na_imputation <- function(column)
{
  column_mean <- mean(column, na.rm = T)
  column <- ifelse(is.na(column), column_mean, column)
}


temp <- df[, !names(df) %in% c('ID', 'obs_date', 'default')]
temp <-
  as.data.frame(apply(temp, MARGIN = 2,  FUN = remove_outliers))
temp <- as.data.frame(apply(temp, MARGIN = 2,  FUN = na_imputation))




# visualization -----------------------------------------------------------
histogram(bins = 10)

temp %>%
  pivot_longer(cols = colnames(temp)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap( ~ name, scales = 'free')
temp %>%
  pivot_longer(cols = colnames(temp)) %>%
  ggplot(aes(y = value)) +
  geom_violin(aes(x = name)) +
  facet_wrap(~name, scales = 'free')


# splitting data  ---------------------------------------------------------
temp <- cbind(df[, names(df) %in% c('ID', 'obs_date', 'default')], temp)
df_default <- temp[temp$default == 1, ]
df_no_default <- temp[temp$default == 0, ]

for (i in 1:10) {
  assign(paste0('df_sample_', i),
         rbind(df_no_default %>% sample_n(length(
           df_default$default
         )), df_default))
}



