banknew = read.csv("banknew.csv")   #banknew為mice補完值後匯出的檔案

#資料前處理
banknew[,1] = as.integer(banknew[,1])
for (i in 2:10) {
  banknew[,i] = as.factor(banknew[,i])
}
banknew[,14] = as.factor(banknew[,14])
banknew[,20] = as.factor(banknew[,20])

#cut the data into 2 sets
set.seed(1)
index = sample(1:nrow(banknew),0.8*ceiling(nrow(banknew)))
train = banknew[index,]
test = banknew[-index,]

#SMOTE
library(DMwR)
#set.seed(1)
#train_smote = SMOTE(y ~ .,train , perc.over = 600,perc.under = 100)
train_smote = read.csv("train_smote.csv")   #train_smote.csv為將training data smote完值後匯出的檔案
train_smote = train_smote[,-1]

table(train_smote$y)

#資料前處理
for (i in 2:10) {
  train_smote[,i] = as.factor(train_smote[,i])
}
train_smote[,14] = as.factor(train_smote[,14])
train_smote[,20] = as.factor(train_smote[,20])


#處理類別型變數
job_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 11)
job_matrix_test = matrix(NA, nrow = nrow(test),ncol = 11)
mar_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 3)
mar_matrix_test = matrix(NA, nrow = nrow(test),ncol = 3)
edu_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 7)
edu_matrix_test = matrix(NA, nrow = nrow(test),ncol = 7)
mon_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 10)
mon_matrix_test = matrix(NA, nrow = nrow(test),ncol = 10)
day_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 5)
day_matrix_test = matrix(NA, nrow = nrow(test),ncol = 5)
pou_matrix_train = matrix(NA, nrow = nrow(train_smote),ncol = 3)
pou_matrix_test = matrix(NA, nrow = nrow(test),ncol = 3)

colnames(job_matrix_train) = levels(train$job)
colnames(job_matrix_test) = levels(train$job)
colnames(mar_matrix_train) = levels(train$marital)
colnames(mar_matrix_test) = levels(train$marital)
colnames(edu_matrix_train) = levels(train$education)
colnames(edu_matrix_test) = levels(train$education)
colnames(mon_matrix_train) = levels(train$month)
colnames(mon_matrix_test) = levels(train$month)
colnames(day_matrix_train) = levels(train$day_of_week)
colnames(day_matrix_test) = levels(train$day_of_week)
colnames(pou_matrix_train) = levels(train$poutcome)
colnames(pou_matrix_test) = levels(train$poutcome)

for(i in 1:11){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,2] == colnames(job_matrix_train)[i]){
      job_matrix_train[j,i] = 1
    }
    else
      job_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,2] == colnames(job_matrix_test)[i]){
      job_matrix_test[j,i] = 1
    }
    else
      job_matrix_test[j,i] = 0
  }
}
  
for(i in 1:3){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,3] == colnames(mar_matrix_train)[i]){
      mar_matrix_train[j,i] = 1
    }
    else
      mar_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,3] == colnames(mar_matrix_test)[i]){
      mar_matrix_test[j,i] = 1
    }
    else
      mar_matrix_test[j,i] = 0
  }
}

for(i in 1:7){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,4] == colnames(edu_matrix_train)[i]){
      edu_matrix_train[j,i] = 1
    }
    else
      edu_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,4] == colnames(edu_matrix_test)[i]){
      edu_matrix_test[j,i] = 1
    }
    else
      edu_matrix_test[j,i] = 0
  }
}

for(i in 1:10){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,9] == colnames(mon_matrix_train)[i]){
      mon_matrix_train[j,i] = 1
    }
    else
      mon_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,9] == colnames(mon_matrix_test)[i]){
      mon_matrix_test[j,i] = 1
    }
    else
      mon_matrix_test[j,i] = 0
  }
}

for(i in 1:5){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,10] == colnames(day_matrix_train)[i]){
      day_matrix_train[j,i] = 1
    }
    else
      day_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,10] == colnames(day_matrix_test)[i]){
      day_matrix_test[j,i] = 1
    }
    else
      day_matrix_test[j,i] = 0
  }
}

for(i in 1:3){
  for(j in 1:nrow(train_smote)){
    if(train_smote[j,14] == colnames(pou_matrix_train)[i]){
      pou_matrix_train[j,i] = 1
    }
    else
      pou_matrix_train[j,i] = 0
  }
  for(j in 1:nrow(test)){
    if(test[j,14] == colnames(pou_matrix_test)[i]){
      pou_matrix_test[j,i] = 1
    }
    else
      pou_matrix_test[j,i] = 0
  }
}

train_x = train_smote[,-c(2,3,4,9,10,14,20)]
train_y = as.matrix(as.numeric(train_smote[,20])) -1
for(i in 2:5){
  train_x[,i] = as.numeric(train_x[,i]) - 1
}
train_x = as.matrix(train_x)

test_x = test[,-c(2,3,4,9,10,14,20)]
test_y = as.matrix(as.numeric(test[,20])) -1
for(i in 2:5){
  test_x[,i] = as.numeric(test_x[,i]) - 1
}
test_x = as.matrix(test_x)
train_x = cbind(train_x, job_matrix_train, mar_matrix_train, edu_matrix_train, mon_matrix_train, day_matrix_train, pou_matrix_train)
test_x = cbind(test_x, job_matrix_test, mar_matrix_test, edu_matrix_test, mon_matrix_test, day_matrix_test, pou_matrix_test)

#adaptive LASSO
library(glmnet)
source("mylars(adalasso使用的函數).r")
source("adalasso函數(選變數).r")
scaled_train_x = apply(train_x, 2, scale)
uncenter_train_y = train_y - mean(train_y)
set.seed(11)
ad_lasso = adalasso(scaled_train_x, uncenter_train_y, k = 5)
colnames(train_x)[which(ad_lasso$coefficients.adalasso != 0)]
sum(ad_lasso$coefficients.adalasso != 0)

#which variable effects the most/less
continuous = c(1,6,7,8,9,10,11,12,13)
#for continuous variables
colnames(train_x)[as.numeric(names(which.max(abs(ad_lasso$coefficients.adalasso[continuous]))))]
ad_lasso$coefficients.adalasso[as.numeric(names(which.max(abs(ad_lasso$coefficients.adalasso[continuous]))))]
#for categoric variables
colnames(train_x)[which.max(abs(coef_adlasso[-c(continuous)]))]
coef_adlasso[which.max(abs(coef_adlasso[-c(continuous)]))]

which.min(abs(ad_lasso$coefficients.adalasso))
ad_lasso$coefficients.adalasso[2]
colnames(train_x)[2]

#coefficient
coef_adlasso = c(1:52)
for(i in 1:52){
  coef_adlasso[i] = ad_lasso$coefficients.adalasso[i]/sd(train_x[,i])
}
x_bar = apply(train_x, 2, mean)
inte_adlasso =  mean(train_y) + ad_lasso$intercept.adalasso - t(coef_adlasso) %*% x_bar
#m = c(1,1,1,1,1,1,1,1,1,1,1,1,1)
inte_adlasso = as.numeric(inte_adlasso)#intercept

#predict adaptive lasso
pred = coef_adlasso %*% t(test_x) + inte_adlasso
pred_result = array(dim = length(pred))
for(i in 1:length(pred)){
  if(pred[i] > 0.5)
    pred_result[i] = "yes"
  else
    pred_result[i] = "no"
}

#accuracy
table(pred_result,test$y)
mean(pred_result == test$y)

#ROC
library(ROCit)
pred = as.numeric(pred)
test_y = as.vector(test_y)
rocit_obj = rocit(score = pred, class = test_y)
rocit_obj$AUC
plot(rocit_obj)

library(pROC)
pred  = as.vector(pred)
proc_obj = roc(response = test_y, predictor = pred)
thr_adlasso = proc_obj$thresholds[which.max(proc_obj$sensitivities+proc_obj$specificities)]
plot(proc_obj)
auc(proc_obj)

#predict adaptive lasso by threshold of Youden index
pred_result_adlasso_YI = array(dim = length(pred))
for(i in 1:length(pred)){
  if(pred[i] > thr_adlasso)
    pred_result_adlasso_YI[i] = "yes"
  else
    pred_result_adlasso_YI[i] = "no"
}
table(pred_result_adlasso_YI, test_y)

adaptive_lasso_train = train_x[,c(1,4,5,7,8,10,11,12,13,15,16,17,18,20,21,24,26,27,28,29,30,31,34,35,36,37,40,42,43,44,47,48,49,51,52)]
adaptive_lasso_test = test_x[,c(1,4,5,7,8,10,11,12,13,15,16,17,18,20,21,24,26,27,28,29,30,31,34,35,36,37,40,42,43,44,47,48,49,51,52)]

lasso_train = read.csv("LASSO_result.csv")
lasso_test = read.csv("LASSO_test_x.csv")

#data frame
df_train = as.data.frame(cbind(train_x,train_y))
colnames(df_train)[53] = "y"
df_test = as.data.frame(cbind(test_x, test_y))
colnames(df_test)[53] = "y"
df_train_ad = as.data.frame(cbind(adaptive_lasso_train, train_y))
colnames(df_train_ad)[36] = "y"
df_test_ad = as.data.frame(cbind(adaptive_lasso_test, test_y))
colnames(df_test_ad)[36] = "y"
df_train_la = as.data.frame(cbind(lasso_train[,-1], train_y))
colnames(df_train_la)[38] = "y"
df_test_la = as.data.frame(cbind(lasso_test[,-1], test_y))
colnames(df_test_la)[38] = "y"

#simple linear regression
lm_fit = lm(y~., data = df_train)
pred_lm = predict(lm_fit, newdata = df_test)

#predict lm fit
pred_result_lm = array(dim = length(pred_lm))
for(i in 1:length(pred_lm)){
  if(pred_lm[i] > 0.5)
    pred_result_lm[i] = "yes"
  else
    pred_result_lm[i] = "no"
}
table(pred_result_lm, test_y)

#ROC and AUC
pred_lm = as.numeric(pred_lm)
proc_lm = roc(response = test_y, predictor = pred_lm)
auc(proc_lm)
thr_lm = proc_lm$thresholds[which.max(proc_lm$sensitivities+proc_lm$specificities)]
rocit_lm = rocit(score = pred_lm, class = test_y)
plot(rocit_lm)

#predict again using cut point = threshold by Youden index
pred_result_lm_YI = array(dim = length(pred_lm))
for(i in 1:length(pred_lm)){
  if(pred_lm[i] > thr_lm)
    pred_result_lm_YI[i] = "yes"
  else
    pred_result_lm_YI[i] = "no"
}
table(pred_result_lm_YI, test_y)

#simple linear regression2 by ad LASSO result
lm_fit2 = lm(y~., data = df_train_ad)
pred_lm2 = predict(lm_fit2, newdata = df_test_ad)

#predict lm_fit2
pred_result_lm2 = array(dim = length(pred_lm2))
for(i in 1:length(pred_lm2)){
  if(pred_lm2[i] > 0.5)
    pred_result_lm2[i] = "yes"
  else
    pred_result_lm2[i] = "no"
}
table(pred_result_lm2, test_y)

#ROC and AUC
pred_lm2 = as.numeric(pred_lm2)
proc_lm2 = roc(response = test_y, predictor = pred_lm2)
auc(proc_lm2)
thr_lm2 = proc_lm2$thresholds[which.max(proc_lm2$sensitivities+proc_lm2$specificities)]
rocit_lm = rocit(score = pred_lm2, class = test_y)
plot(rocit_lm)

#predict again using cut point = threshold by Youden index
pred_result_lm2_YI = array(dim = length(pred_lm2))
for(i in 1:length(pred_lm2)){
  if(pred_lm2[i] > thr_lm2)
    pred_result_lm2_YI[i] = "yes"
  else
    pred_result_lm2_YI[i] = "no"
}
table(pred_result_lm2_YI, test_y)

#simple linear regression2 by LASSO result
lm_fit3 = lm(y~., data = df_train_la)
pred_lm3 = predict(lm_fit3, newdata = df_test_la)

#predict lm_fit3
pred_result_lm3 = array(dim = length(pred_lm3))
for(i in 1:length(pred_lm3)){
  if(pred_lm3[i] > 0.5)
    pred_result_lm3[i] = "yes"
  else
    pred_result_lm3[i] = "no"
}
table(pred_result_lm3, test_y)

#ROC and AUC
pred_lm3 = as.numeric(pred_lm3)
proc_lm3 = roc(response = test_y, predictor = pred_lm3)
auc(proc_lm3)
thr_lm3 = proc_lm3$thresholds[which.max(proc_lm3$sensitivities+proc_lm3$specificities)]
rocit_lm3 = rocit(score = pred_lm3, class = test_y)
plot(rocit_lm3)

#predict again using cut point = threshold by Youden index
pred_result_lm3_YI = array(dim = length(pred_lm3))
for(i in 1:length(pred_lm3)){
  if(pred_lm3[i] > thr_lm3)
    pred_result_lm3_YI[i] = "yes"
  else
    pred_result_lm3_YI[i] = "no"
}
table(pred_result_lm3_YI, test_y)
