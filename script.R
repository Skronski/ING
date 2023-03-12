# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(regclass)
library(pROC)
library(glmnet)
library(rpart)
library(rpart.plot)
library(lubridate)
library(performanceEstimation)
library(fastDummies)

path <- 'D:\\Informatyka i ekonometria\\ING'
setwd(path)


# reading data ------------------------------------------------------------

df <- read.csv('credit_sample.csv')

summary(df)

# date transformation -----------------------------------------------------

df$obs_date <-  as.POSIXlt(df$obs_date, format = "%Y-%m-%d")
df$month_x <- sin(2*pi*month(df$obs_date)/12)
df$month_y <- cos(2*pi*month(df$obs_date)/12)



df$quarter <- ifelse(month(df$obs_date)<4,
                     1,ifelse(month(df$obs_date)<7,2,
                              ifelse(month(df$obs_date)<10,3,4)))
df <- dummy_cols(df,'quarter', remove_most_frequent_dummy = T, remove_selected_columns = T)

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

df_vizualization <- temp[,!names(temp) %in% c('quarter_1', 'quarter_2', 'quarter_3', 'month_x', 'month_y')]


# visualization -----------------------------------------------------------

df_vizualization %>%
  pivot_longer(cols = colnames(df_vizualization)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = 'free')
df_vizualization %>%
  pivot_longer(cols = colnames(df_vizualization)) %>%
  ggplot(aes(y = value)) +
  geom_violin(aes(x = name)) +
  facet_wrap( ~ name, scales = 'free')

df_vizualization %>%
  pivot_longer(cols = colnames(df_vizualization)) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(aes(x = name)) +
  facet_wrap( ~ name, scales = 'free')

# splitting data  ---------------------------------------------------------

temp <-
  cbind(df[, names(df) %in% c('ID', 'obs_date', 'default')], temp)


# preprocessing -----------------------------------------------------------

tmp <- preProcess(temp[!names(temp) %in% c('ID', 'obs_date', 'default', 'month_x', 'month_y')], "range")
temp$ID <- NULL
temp$obs_date <- NULL
default <- temp$default
temp$default <- NULL

scaled_df_sample_1 <- predict(tmp, temp)




data_2 <- scaled_df_sample_1[,1:length(scaled_df_sample_1[1,])]^2
names(data_2) <- paste0(colnames(data_2),"_2")

scaled_data_2 <- scaled_df_sample_1

for (i in 1:length(scaled_data_2[1,])) {
  scaled_data_2[,i]<- ifelse(scaled_data_2[,i] <0.01, 0.01,scaled_data_2[,i])
}


dane_log <- log(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_log)
names(dane_log) <- paste0(colnames(dane_log),"_log")






dane_exp <- exp(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_exp)
names(dane_exp) <- paste0(colnames(dane_log),"_exp")


dane_sqrt <- sqrt(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_sqrt)
names(dane_sqrt) <- paste0(colnames(dane_sqrt),"_sqrt")

dane_3<- scaled_data_2[,1:length(scaled_data_2[1,])]^3
head(dane_3)
names(dane_sqrt) <- paste0(colnames(dane_3),"_3")

dane_4 <- scaled_data_2[,1:length(scaled_data_2[1,])]^4
head(dane_4)
names(dane_4) <- paste0(colnames(dane_4),"_4")

dane_sin <- sin(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_sin)
names(dane_sin) <- paste0(colnames(dane_sin),"_sin")
dane_1_x <-1/(scaled_data_2[,1:length(scaled_data_2[1,])])
head(dane_1_x)
names(dane_1_x) <- paste0(colnames(dane_1_x),"_1_x")





df <- cbind.data.frame(dane_log, default,dane_exp, dane_3, dane_4, dane_sin, dane_sqrt, dane_1_x)

# validation set ----------------------------------------------------------

temp <- df %>% sample_frac(.8, replace = F)
validation <- setdiff(df,temp)


# multicollinearity and stepwise selection --------------------------------



train <- temp %>% sample_frac(.7, replace = F)

test <- setdiff(temp,train)


my_model <-
  glm(formula = default~Var_01 + Var_02 + Var_03 + Var_04 + Var_05 + Var_06 +
        Var_07 + Var_08 + Var_09 + Var_10 + Var_11 + Var_12 + Var_13 +
        Var_14 + Var_15 + Var_16 + Var_17 + Var_18 + Var_19 + Var_20 +
        Var_21 + Var_22 + Var_23 + Var_24 + Var_25 + Var_26 + Var_27 +
        Var_28 + Var_29 + Var_30 + Var_31 + Var_32 + Var_33 + Var_34 +
        Var_35 + Var_36 + Var_37 + Var_38 + Var_39 + month_x + month_y +
        quarter_1+quarter_2+ quarter_3,family = 'binomial', data = train)
step_model <- step(my_model)
summary(step_model)
VIF(step_model)



model_vif <-
  glm(
    formula = default ~ Var_02 + Var_03 + Var_04 +
      Var_06 + Var_07 + Var_12 + Var_13 + Var_15 +
      Var_17 + Var_20 + Var_22 + Var_23 + Var_24 +
      Var_25 + Var_26 + Var_27 + Var_28 + Var_31 +
      Var_33 + Var_34 + Var_35 + Var_36 +  Var_39,
    family = "binomial",
    data = train
  )
summary(model_vif)
VIF(model_vif)


# oversampling ------------------------------------------------------------




temp$default <- as.factor(ifelse(temp$default == 1,"rare","common"))

temp$obs_date <- NULL

smote_oversampled <- smote(default~., temp, perc.over = 16)


smote_oversampled$default <- ifelse(smote_oversampled$default == "rare",1,0)

sum(smote_oversampled$default)

write.csv(smote_oversampled,
          file = paste0(path, '\\', 'oversampled_df_smote_01', '.csv'),
          row.names = FALSE)
write.csv(validation,
          file = paste0(path, '\\', 'validation_01', '.csv'),
          row.names = FALSE)




smote_oversampled <- read.csv(paste0(path, '\\', 'oversampled_df_smote_01', '.csv'))
validation <- read.csv(paste0(path, '\\', 'validation_01', '.csv'))


train <- smote_oversampled %>% sample_frac(.7, replace = F)
test <- setdiff(smote_oversampled,train)


# vif logistic ------------------------------------------------------------




model_vif <-
  glm(
    formula = default ~ Var_02 + Var_03 + Var_04 +
      Var_06 + Var_07 + Var_12 + Var_13 + Var_15 +
      Var_17 + Var_20 + Var_22 + Var_23 + Var_24 +
      Var_25 + Var_26 + Var_27 + Var_28 + Var_31 +
      Var_33 + Var_34 + Var_35 + Var_36 +  Var_39,
    family = "binomial",
    data = train
  )
summary(model_vif)
VIF(model_vif)


test_pred <- predict(model_vif,test[!names(test) %in% c('default')], type = 'response')

confmatrix <- confusionMatrix(data = as.factor(as.numeric(test_pred>0.5)), reference = as.factor(test$default))

auc(test$default, test_pred)
roc_score_logit_vif <- roc(test$default, test_pred)

treshold <- coords(roc_score_logit_vif, "best", ret = "threshold")

validation_pred <- predict(model_vif,validation[!names(validation) %in% c('default')], type = 'response')

confmatrix_valid <- confusionMatrix(data = as.factor(as.numeric(validation_pred>treshold)), reference = as.factor(validation$default))
confmatrix_valid
auc(validation$default, validation_pred)
# tree --------------------------------------------------------------------



fit.tree <-
  rpart(
    default ~ Var_01 + Var_02 + Var_03 + Var_04 + Var_05 +
      Var_06 + Var_07 + Var_08 + Var_09 + Var_10 + Var_11 +
      Var_12 + Var_13 + Var_14 + Var_15 + Var_16 + Var_17 +
      Var_18 + Var_19 + Var_20 + Var_21 + Var_22 + Var_23 +
      Var_24 + Var_25 + Var_26 + Var_27 + Var_28 + Var_29 +
      Var_30 + Var_31 + Var_32 + Var_33 + Var_34 + Var_35 +
      Var_36 + Var_37 + +Var_38 + Var_39 + quarter_1 + quarter_2 + quarter_3,
    data = train,
    method = "class",
    cp = 0.008
  )

fit.tree
rpart.plot(fit.tree)


vi_tree <- fit.tree$variable.importance

barplot(vi_tree, horiz = TRUE, las = 1)




pred.tree = predict(fit.tree, test, type = "prob")

table(pred.tree,test$default)


auc(test$default, pred.tree[,1])


validation_pred <- predict(fit.tree, validation, type = "prob")

roc_score <- roc(validation$default, validation_pred[,1])
treshold <- coords(roc_score, "best", ret = "threshold")
validation_pred <- predict(fit.tree, validation, type = "class")
confmatrix_valid <- confusionMatrix(data = validation_pred, reference = as.factor(validation$default))
confmatrix_valid
validation_pred <- predict(fit.tree, validation, type = "prob")

auc(validation$default, validation_pred[,1])


# lasso -------------------------------------------------------------------



lambdas_to_try <- 10^seq(-3, 5, length.out = 100)


train_matrix <- as.matrix(train[, !names(train)%in% c('default')])
y <- train$default

cv_model <- cv.glmnet(train_matrix,y, alpha = 1, lambda = lambdas_to_try)
best_lambda <- cv_model$lambda.min
model_lasso <- glmnet(train_matrix,y, alpha = 1,  lambda = best_lambda, family = 'binomial')

test_pred_lasso <- predict(model_lasso, s = best_lambda, newx = as.matrix(test[,!names(test)%in% c('default')]), type = 'response')

auc(test$default, test_pred_lasso)


validation_pred_lasso <- predict(model_lasso, s = best_lambda, newx = as.matrix(validation[,!names(validation)%in% c('default')]), type = 'response')
auc(validation$default, validation_pred_lasso)

roc_score_lasso <- roc(test$default, test_pred_lasso)



roc_score_lasso_validation <- roc(validation$default, validation_pred_lasso)

validation_pred_lasso <- predict(model_lasso, s = best_lambda, newx = as.matrix(validation[,!names(validation)%in% c('default')]), type = 'response')
treshold <- coords(roc_score_lasso, "best", ret = "threshold")


confmatrix_valid_lasso <- confusionMatrix(data = as.factor(as.numeric(validation_pred_lasso>treshold[,1])), reference = as.factor(validation$default))
confmatrix_valid_lasso





auc(validation$default, validation_pred_lasso)




# roc curve ---------------------------------------------------------------

roc_score_tree <- roc(test$default, pred.tree[,1])

plot(roc_score_logit_vif)
plot(roc_score_tree, add = TRUE,col = 'green')
plot(roc_score_lasso, add = TRUE,col = 'red')
legend(0.4,0.4, legend=c("LASSO", "TREE", "LOGISTIC \nREGRESSION"),
       col=c("red", "green", "black"), lty=1, cex=0.8, box.lty=0)
 abline(v = 1)
abline(h=0)
abline(h=1)


# roc curve validation ----------------------------------------------------



validation_pred_vif <- predict(model_vif,validation[!names(validation) %in% c('default')], type = 'response')
roc_score_logit_vif_validation <- roc(validation$default, validation_pred_vif)

validation_pred_tree <- predict(fit.tree, validation, type = "prob")

roc_score_tree_validation  <- roc(validation$default, validation_pred_tree[,1])
roc_score_lasso_validation <- roc(validation$default, validation_pred_lasso[,1])

plot(roc_score_logit_vif_validation)
plot(roc_score_tree_validation, add = TRUE,col = 'green')
plot(roc_score_lasso_validation, add = TRUE,col = 'red')
legend(0.4,0.4, legend=c("LASSO", "TREE", "LOGISTIC \nREGRESSION"),
       col=c("red", "green", "black"), lty=1, cex=0.8, box.lty=0)
abline(v = 1)
abline(h=0)
abline(h=1)




# crossvalidation ---------------------------------------------------------
k_fold <- 1
columns = c("auc_lasso","auc_logistic","auc_tree")
df_auc = data.frame(matrix(nrow = k_fold, ncol = length(columns)))
colnames(df_auc) = columns


lambdas_to_try <- 10 ^ seq(-3, 5, length.out = 100)

for (i in 1:k_fold)
{
  train <- smote_oversampled %>% sample_frac(.8, replace = F)
  test <- setdiff(smote_oversampled, train)

  model_vif <-
    glm(
      formula = default ~ Var_02 + Var_03 + Var_04 +
        Var_06 + Var_07 + Var_12 + Var_13 + Var_15 +
        Var_17 + Var_20 + Var_22 + Var_23 + Var_24 +
        Var_25 + Var_26 + Var_27 + Var_28 + Var_31 +
        Var_33 + Var_34 + Var_35 + Var_36 +  Var_39,
      family = "binomial",
      data = train
    )
  test_pred <- predict(model_vif,test[!names(test) %in% c('default')], type = 'response')

  df_auc$auc_logistic[i] <-  auc(test$default, test_pred)

  #lasso


  train_matrix <- as.matrix(train[, !names(train)%in% c('default')])
  y <- train$default

  cv_model <- cv.glmnet(train_matrix,y, alpha = 1, lambda = lambdas_to_try)
  best_lambda <- cv_model$lambda.min
  model_lasso <- glmnet(train_matrix,y, alpha = 1,  lambda = best_lambda, family = 'binomial')

  test_pred_lasso <- predict(model_lasso, s = best_lambda, newx = as.matrix(test[,!names(test)%in% c('default')]), type = 'response')
  df_auc$auc_lasso[i] <-  auc(test$default, test_pred_lasso)

  # tree
  fit.tree <-
    rpart(
      default ~ Var_01 + Var_02 + Var_03 + Var_04 + Var_05 +
        Var_06 + Var_07 + Var_08 + Var_09 + Var_10 + Var_11 +
        Var_12 + Var_13 + Var_14 + Var_15 + Var_16 + Var_17 +
        Var_18 + Var_19 + Var_20 + Var_21 + Var_22 + Var_23 +
        Var_24 + Var_25 + Var_26 + Var_27 + Var_28 + Var_29 +
        Var_30 + Var_31 + Var_32 + Var_33 + Var_34 + Var_35 +
        Var_36 + Var_37 + +Var_38 + Var_39 + quarter_1 + quarter_2 + quarter_3,
      data = train,
      method = "class",
      cp = 0.008
    )

  pred.tree = predict(fit.tree, test, type = "prob")


  df_auc$auc_tree[i] <-  auc(test$default, pred.tree[,1])

  print(i)
}

ggplot() +
  geom_boxplot(aes("AUC \n Decision Trees",df_auc$auc_tree))+
  geom_boxplot(aes("AUC logistic regression\n with LASSO",df_auc$auc_lasso))+
  geom_boxplot(aes("AUC Logistic Regression",df_auc$auc_logistic))+
  xlab("AUC factor")+
  ylab("Score")+
  ggtitle(paste0(k_fold,"-fold crossvalidation auc scores"))



