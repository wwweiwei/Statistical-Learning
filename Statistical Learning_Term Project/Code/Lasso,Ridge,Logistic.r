rm(list = ls())

library(readr)
library(dplyr)
library(glmnet)
library(DMwR)
library(caret)
library(pROC)

banknew <- read_csv("banknew.csv")  #banknew為mice補完值後匯出的檔案
banknew <- as.data.frame(banknew)
banknew[,1] <-  as.integer(banknew[,1])
for (i in 2:10) {
  banknew[,i] <-  as.factor(banknew[,i])
}
banknew[,14] <-  as.factor(banknew[,14])
banknew[,20] <-  as.factor(banknew[,20])

set.seed(1)
index <-  sample(1:nrow(banknew), 0.8*ceiling(nrow(banknew)))
test <-  banknew[-index,]
test$pdays <- ifelse(test$pdays == 999, 0, test$pdays)
table(test$y)

train_smote <- read_csv("train_smote.csv")  #train_smote.csv為將training data smote完值後匯出的檔案
train_smote <- train_smote[, -1] %>% as.data.frame()
train_smote[,1] <-  as.integer(train_smote[,1])
for (i in 2:10) {
  train_smote[,i] <-  as.factor(train_smote[,i])
}
train_smote[,14] <-  as.factor(train_smote[,14])
train_smote[,20] <-  as.factor(train_smote[,20])

train_smote$pdays <- ifelse(train_smote$pdays == 999, 0, train_smote$pdays)
table(train_smote$y)

############################################################
#----------------------- logistic -------------------------#
############################################################
#----all variable----

set.seed(1)
fit <- glm(formula = y~., data = train_smote, family = "binomial")
summary(fit)
fit_predicted <- predict(fit, newdata = test[, -20], type = "response")
fit_predict_class <- ifelse(fit_predicted >= 0.5, "yes", "no") #937/(937+7301)
mean(test$y == fit_predict_class)
fit$coefficients

table(fit_predict_class, test$y)
par(pty = "s")
roc(test$y, fit_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "Logisitic Regression with All Variables", lwd = 4, print.auc = TRUE)
class(fit_predicted)

#----lasso results----
train_x <- read_csv("train_x.csv")  #train_x.csv為將train_smote中類別型變數拆開成不同變數，最後匯出的檔案
train_x$pdays <- ifelse(train_x$pdays == 999, 0, train_x$pdays)
test_x$pdays <- ifelse(test_x$pdays == 999, 0, test_x$pdays)
lasso_result <- train_x[,c("age", "loan", "contact", "pdays", "previous", "cons.price.idx", "cons.conf.idx", 
                           "nr.employed", "blue-collar", "entrepreneur", "housemaid", "management", "self-employed",
                           "services", "student", "unemployed", "married", "single", "basic.4y", "basic.6y", "basic.9y",
                           "high.school", "illiterate", "university.degree", "apr", "aug", "dec", "jul", "mar", "nov",
                           "oct", "sep", "thu", "tue", "wed", "nonexistent", "success")]
test_x_lasso <- test_x[,c("age", "loan", "contact", "pdays", "previous", "cons.price.idx", "cons.conf.idx", 
                          "nr.employed", "blue-collar", "entrepreneur", "housemaid", "management", "self-employed",
                          "services", "student", "unemployed", "married", "single", "basic.4y", "basic.6y", "basic.9y",
                          "high.school", "illiterate", "university.degree", "apr", "aug", "dec", "jul", "mar", "nov",
                          "oct", "sep", "thu", "tue", "wed", "nonexistent", "success")]
lasso_result <- cbind.data.frame(lasso_result, y = train_smote$y)

set.seed(1)
fit_lasso <- glm(formula = y~., data = lasso_result, family = "binomial")
fit_lasso_predicted <- predict(fit_lasso, newdata = test_x_lasso, type = "response")
fit_lasso_predict_class <- ifelse(fit_lasso_predicted >= 0.5, "yes", "no") #937/(937+7301)
mean(test$y == fit_lasso_predict_class)
table(fit_lasso_predict_class, test$y)
par(pty = "s")
roc(test$y, fit_lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "Logisitic Regression with LASSO Results", lwd = 4, print.auc = TRUE)

#----adaptive lasso results----
ad_lasso_train <- read_csv("ad_lasso_train_x.csv") #ad_lasso_train_x.csv將train_smote中被adaptive lasso選到的變數匯出的檔案
ad_lasso_train$pdays <- ifelse(ad_lasso_train$pdays == 999, 0, ad_lasso_train$pdays)
ad_laaso_result <- cbind.data.frame(ad_lasso_train, y = train_smote$y)

set.seed(1)
fit_ad_lasso <- glm(formula = y~., data = ad_laaso_result, family = "binomial")
ad_lasso_test <- read_csv("ad_lasso_test_x.csv")  #ad_lasso_test_x.csv將test中被adaptive lasso選到的變數匯出的檔案
ad_lasso_test$pdays <- ifelse(ad_lasso_test$pdays == 999, 0, ad_lasso_test$pdays)

fit_ad_lasso_predicted <- predict(fit_ad_lasso, newdata = ad_lasso_test, type = "response")
fit_ad_lasso_predict_class <- ifelse(fit_ad_lasso_predicted >= 0.5, "yes", "no") #937/(937+7301)
mean(test$y == fit_ad_lasso_predict_class)
table(fit_ad_lasso_predict_class, test$y)
par(pty = "s")
roc(test$y, fit_ad_lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "Logisitic Regression with Adaptive LASSO Results", lwd = 4, print.auc = TRUE)


#########################################################
#----------------------- lasso -------------------------#
#########################################################
set.seed(1)
lasso_cv <- cv.glmnet(x = as.matrix(train_x), y = train_smote$y, family = "binomial", alpha = 1, nfolds = 10)
par(pty = "m")
plot(lasso_cv)

lasso_cv$lambda.min
lasso_cv$lambda.1se

lasso <- glmnet(x = as.matrix(train_x), y = train_smote$y, family = "binomial", alpha = 1, lambda = lasso_cv$lambda.1se)

lasso_predicted <- predict(lasso, newx = as.matrix(test_x), type = "response")
lasso_predict_class <- ifelse(lasso_predicted >= 0.5, "yes", "no")
mean(test$y == lasso_predict_class)
table(lasso_predict_class, test$y)
par(pty = "s")
roc(test$y, lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "LASSO", lwd = 4, print.auc = TRUE)
lasso$beta

write.csv(lasso_result, file = "LASSO_result.csv")


#----adaptive lasso results----
set.seed(1)
lasso_ad_lasso_cv <- cv.glmnet(x = as.matrix(ad_lasso_train), y = train_smote$y, family = "binomial", alpha = 1, nfolds = 10)
lasso_ad_lasso <- glmnet(x = as.matrix(ad_lasso_train), y = train_smote$y, family = "binomial", alpha = 1, lambda = lasso_ad_lasso_cv$lambda.1se)
lasso_ad_lasso_predicted <- predict(lasso_ad_lasso, newx = as.matrix(ad_lasso_test), type = "response")
lasso_ad_lasso_predict_class <- ifelse(lasso_ad_lasso_predicted >= 0.5, "yes", "no")
mean(test$y == lasso_ad_lasso_predict_class)
table(lasso_ad_lasso_predict_class, test$y)
par(pty = "s")
roc(test$y, lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "LASSO with Adaptive LASSO Results", lwd = 4, print.auc = TRUE)

#########################################################
#----------------------- ridge -------------------------#
#########################################################
set.seed(1)
ridge_cv <- cv.glmnet(x = as.matrix(train_x), y = train_smote$y, family = "binomial", alpha = 0, nfolds = 10)
plot(ridge_cv)
ridge_cv$lambda.min
ridge_cv$lambda.1se

ridge <- glmnet(x = as.matrix(train_x), y = train_smote$y, family = "binomial", alpha = 0, lambda = ridge_cv$lambda.min)
ridge_predicted <- predict(ridge, newx = as.matrix(test_x), type = "response")
ridge_predict_class <- ifelse(ridge_predicted >= 0.5, "yes", "no")
mean(test$y == ridge_predict_class)
table(ridge_predict_class, test$y)
par(pty = "s")
roc(test$y, ridge_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "RIDGE", lwd = 4, print.auc = TRUE)

#----lasso results----
set.seed(1)
ridge_lasso_cv <- cv.glmnet(x = as.matrix(lasso_result[, -38]), y = lasso_result[, 38], family = "binomial", alpha = 0, nfolds = 10)
ridge_lasso <- glmnet(x = as.matrix(lasso_result[, -38]), y = lasso_result[, 38], family = "binomial", alpha = 0, lambda = ridge_lasso_cv$lambda.min)
ridge_lasso_predicted <- predict(ridge_lasso, newx = as.matrix(test_x_lasso), type = "response")
ridge_laso_predict_class <- ifelse(ridge_lasso_predicted >= 0.5, "yes", "no")
mean(test$y == ridge_laso_predict_class)
table(ridge_laso_predict_class, test$y)
par(pty = "s")
roc(test$y, ridge_lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "RIDGE with LASSO results", lwd = 4, print.auc = TRUE)


#----adaptive lasso results----
set.seed(1)
ridge_ad_lasso_cv <- cv.glmnet(x = as.matrix(ad_lasso_train), y = train_smote$y, family = "binomial", alpha = 0, nfolds = 10)
ridge_ad_lasso <- glmnet(x = as.matrix(ad_lasso_train), y = train_smote$y, family = "binomial", alpha = 0, lambda = ridge_ad_lasso_cv$lambda.min)

ridge_ad_lasso_predicted <- predict(ridge_ad_lasso, newx = as.matrix(ad_lasso_test), type = "response")
ridge_ad_laso_predict_class <- ifelse(ridge_ad_lasso_predicted >= 0.5, "yes", "no")
mean(test$y == ridge_ad_laso_predict_class)
table(ridge_ad_laso_predict_class, test$y)
par(pty = "s")
roc(test$y, ridge_lasso_predicted, plot = TRUE, legacy.axes = TRUE, pecent = TRUE, 
    xlab = "False Positive Pecentage", ylab ="True Positive Percentage", 
    main = "RIDGE with Adaptive LASSO Results", lwd = 4, print.auc = TRUE)