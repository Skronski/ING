# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(regclass)
library(pROC)
library(glmnet)





path <- 'D:\\Informatyka i ekonometria\\ING'
setwd(path)


# reading data ------------------------------------------------------------

df <- read.csv('credit_sample.csv')

summary(df)
df$obs_date <-  as.Date(df$obs_date, format = "%Y-%m-%d")


# outliers, missing values ------------------------------------------------

stat <- summary(df)

nas <- as.data.frame(stat[7,])


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


temp <- df[,!names(df) %in% c('ID', 'obs_date', 'default')]
temp <-
  as.data.frame(apply(temp, MARGIN = 2,  FUN = remove_outliers))
temp <- as.data.frame(apply(temp, MARGIN = 2,  FUN = na_imputation))




# visualization -----------------------------------------------------------

temp %>%
  pivot_longer(cols = colnames(temp)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = 'free')
temp %>%
  pivot_longer(cols = colnames(temp)) %>%
  ggplot(aes(y = value)) +
  geom_violin(aes(x = name)) +
  facet_wrap( ~ name, scales = 'free')

temp %>%
  pivot_longer(cols = colnames(temp)) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(aes(x = name)) +
  facet_wrap( ~ name, scales = 'free')

# splitting data  ---------------------------------------------------------
temp <-
  cbind(df[, names(df) %in% c('ID', 'obs_date', 'default')], temp)
df_default <- temp[temp$default == 1,]
df_no_default <- temp[temp$default == 0,]

for (i in 1:10) {
  assign(paste0('df_sample_', i),
         rbind(df_no_default %>% sample_n(length(
           df_default$default
         )), df_default))
}
for (i in 1:10) {
  write.csv(get(paste0('df_sample_', i)),
            file = paste0(path, '\\', paste0('df_sample_', i), '.csv'),
            row.names = FALSE)
}



# preprocessing -----------------------------------------------------------

tmp <- preProcess(df_sample_1[!names(df_sample_1) %in% c('ID', 'obs_date', 'default')], "range")
scaled_df_sample_1 <- predict(tmp, df_sample_1)

scaled_df_sample_1$ID <- NULL
scaled_df_sample_1$obs_date <- NULL
scaled_df_sample_1$default <- NULL




data_2 <- scaled_df_sample_1[,1:length(scaled_df_sample_1[1,])]^2
names(data_2) <- paste0(colnames(data_2),"_2")

scaled_data_2 <- scaled_df_sample_1

for (i in 1:length(scaled_data_2[1,])) {
  scaled_data_2[,i]<- ifelse(scaled_data_2[,i] <0.01, 0.01,scaled_data_2[,i])
}


dane_log <- log(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_log)
names(dane_log) <- paste0(colnames(dane_log),"_log")

default <- df_sample_1$default
df <- cbind.data.frame(dane_log,data_2, scaled_df_sample_1, default)


# test/train --------------------------------------------------------------

train <- scaled_df_sample_1 %>% sample_frac(.7, replace = F)

test <- setdiff(scaled_df_sample_1,train)




my_model <-
  glm(formula = default~.,family = 'binomial', data = train)
step_model <- step(my_model)
summary(step_model)
VIF(step_model)

model_vif <- glm(formula = default ~ Var_01  + Var_03 + Var_05 + Var_06 +
      Var_10  + Var_12 + Var_13 + Var_14 + Var_15 + Var_17 +
      Var_20 + Var_22 + Var_23 + Var_24 + Var_25 + Var_27 + Var_29 +
      Var_31 + Var_36 + Var_38 + Var_39, family = "binomial",
    data = train)
summary(model_vif)
VIF(model_vif)


test_pred <- predict(model_vif,test[!names(test) %in% c('default')], type = 'response')

confmatrix <- confusionMatrix(data = as.factor(as.numeric(test_pred>0.5)), reference = as.factor(test$default))




auc(test$default, test_pred)
roc_score <- roc(test$default, test_pred)
plot(roc_score)




# lasso -------------------------------------------------------------------

train <- df %>% sample_frac(.7, replace = F)

test <- setdiff(df,train)

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)


train_matrix <- as.matrix(train[, !names(train)%in% c('default')])
y <- train$default

cv_model <- cv.glmnet(train_matrix,y, alpha = 1, lambda = lambdas_to_try)
best_lambda <- cv_model$lambda.min
model_lasso <- glmnet(train_matrix,y, alpha = 1,  lambda = best_lambda, family = 'binomial')

test_pred <- predict(model_lasso, s = best_lambda, newx = as.matrix(test[,!names(test)%in% c('default')]), type = 'response')


auc(test$default, test_pred)
roc_score <- roc(test$default, test_pred)
plot(roc_score)

