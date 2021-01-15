# install.packages("MASS")
library(MASS)
# install.packages("class")
library(class)
#install.packages('DMwR')
library(DMwR)
library(leaps)
#install.packages('pROC')
library(pROC)
#install.packages('ROCR')
library(ROCR)
library(e1071)

#將資料作處理，把類別型變數level訂好
banknew = read.csv("banknew.csv")   #banknew為mice補完值後匯出的檔案
for (i in 2:10) {
  banknew[,i] = as.factor(banknew[,i])
}
banknew[,14] = as.factor(banknew[,14])
banknew[,20] = as.factor(banknew[,20])
set.seed(1)
index = sample(1:nrow(banknew),0.8*ceiling(nrow(banknew)))
train = banknew[index,]
test = banknew[-index,]
set.seed(1)
train = SMOTE(y ~ .,train , perc.over = 600,perc.under = 100)
trainy=train$y
testy=test$y
train_x_lasso=read.csv('LASSO_train_x.csv')[,-1]  ##LASSO_train_x.csv為將train_x中被lasso選到的變數匯出的檔案
test_x_lasso=read.csv('LASSO_test_x.csv')[,-1]  ##LASSO_test_x.csv為將test_x中被lasso選到的變數匯出的檔案
train_lasso=cbind(trainy,train_x_lasso)
test_lasso=cbind(testy,test_x_lasso)


for (i in c(3,4,10:38)){
  train_lasso[,i]=as.factor(train_lasso[,i])
}
for (i in c(3,4,10:38)){
  test_lasso[,i]=as.factor(test_lasso[,i])
}

#-------------------------------------LDA做預測----------------------------------
fit_lda=lda(trainy~.,data=train_lasso)
fit_lda
fit_lda$prior
pred_lda=predict(fit_lda,test_lasso)
table(pred_lda$class,test_lasso$testy)
#LDA method accuracy=0.859
mean(pred_lda$class==test_lasso$testy)
pred_lda$posterior
#ROC curve for LDA
roc_lda=roc(test_lasso$testy, pred_lda$posterior[,2], plot = T,auc=T)
roc_lda$auc
title(main='ROC Curve for LDA (AUC=0.7515)')
#Youden's Index to choose cut point
lda_threshold=roc_lda$thresholds[which.max(roc_lda$sensitivities+
                                             roc_lda$specificities-1)]
lda_threshold
pred_lda1=rep(NA,nrow(test_lasso))
pred_lda1=ifelse(pred_lda$posterior[,2]>=lda_threshold,'yes','no')
table(pred_lda1,test_lasso$testy)

#-------------------------------------QDA做預測----------------------------------
fit_qda=qda(trainy~.,data=train_lasso)
fit_qda
pred_qda=predict(fit_qda,test_lasso)
table(pred_qda$class,test_lasso$testy)
#QDA method accuracy=0.875
mean(pred_qda$class==test_lasso$testy)
#ROC curve for QDA
roc_qda=roc(test_lasso$testy, as.numeric(pred_qda$posterior[,2]), plot = T,auc=T)
roc_qda$auc
title(main='ROC Curve for QDA (AUC=0.7411)')
#Youden's Index to choose cut point
qda_threshold=roc_qda$thresholds[which.max(roc_qda$sensitivities+
                                             roc_qda$specificities-1)]
qda_threshold
pred_qda1=rep(NA,nrow(test))
pred_qda1=ifelse(pred_qda$posterior[,2]>=qda_threshold,'yes','no')
table(pred_qda1,test_lasso$testy)

#--------------------------------------Naive Bayes做預測-----------------------------
fit_nb=naiveBayes(trainy~.,data=train_lasso)
#NB method accuracy=0.852
pred_nb2=predict(fit_nb,test_lasso)
table(pred_nb2,test_lasso$testy)
mean(pred_nb2==test_lasso$testy)

pred_nb = predict(fit_nb, newdata = test_lasso, type="raw")
pred_nb = pred_nb[, 2]
View(pred_nb)
#ROC curve for Naive Bayes
roc_nb=roc(test_lasso$testy,pred_nb,plot=T,auc=T)
roc_nb$auc
title(main='ROC Curve for Naive Bayes method (AUC=0.7563)')
#Youden's Index to choose cut point
nb_threshold=roc_nb$thresholds[which.max(roc_nb$sensitivities+
                                           roc_nb$specificities-1)]
nb_threshold
pred_nb1=rep(NA,nrow(test))
pred_nb1=ifelse(pred_nb>=nb_threshold,'yes','no')
table(pred_nb1,test_lasso$testy)
