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
train_x=read.csv('train_x.csv')   #train_x.csv為將train_smote中類別型變數拆開成不同變數，最後匯出的檔案
test_x=read.csv('test_x.csv')   #test_x.csv為將tset中類別型變數拆開成不同變數，最後匯出的檔案
train=cbind(trainy,train_x)
test=cbind(testy,test_x)
View(train)
View(test)



for (i in c(3:6,15:53)){
  train[,i]=as.factor(train[,i])
}
for (i in c(3:6,15:53)){
  test[,i]=as.factor(test[,i])
}

lassomodel=c(2,5,6,8,9,11,12,13,14,16,17,18,19,21,22,25,27,28,29,30,31,32,35,36,
             37,38,41,43,44,45,48,49,50,52,53)

#--------------------------LASSO Model-------------------------------
#--------------------------LDA做預測------------------------------------
fit_lda=lda(trainy~.,data=train[,c(1,lassomodel)])
pred_lda=predict(fit_lda,test)
table(pred_lda$class,test$testy)
#ROC curve for LDA
roc_lda=roc(test$testy, pred_lda$posterior[,2], plot = T,auc=T)
roc_lda$auc
title(main='ROC Curve for LDA (AUC=0.7511)')
#Youden's Index to choose cut point
lda_threshold=roc_lda$thresholds[which.max(roc_lda$sensitivities+
                                             roc_lda$specificities-1)]
lda_threshold
pred_lda1=rep(NA,nrow(test))
pred_lda1=ifelse(pred_lda$posterior[,2]>=lda_threshold,'yes','no')
table(pred_lda1,test$testy)

#--------------------------QDA做預測------------------------------------
fit_qda=qda(trainy~.,data=train[,c(1,lassomodel)])
pred_qda=predict(fit_qda,test)
table(pred_qda$class,test$testy)
#ROC curve for QDA
roc_qda=roc(test$testy, as.numeric(pred_qda$posterior[,2]), plot = T,auc=T)
roc_qda$auc
title(main='ROC Curve for QDA (AUC=0.7445)')
#Youden's Index to choose cut point
qda_threshold=roc_qda$thresholds[which.max(roc_qda$sensitivities+
                                             roc_qda$specificities-1)]
qda_threshold
pred_qda1=rep(NA,nrow(test))
pred_qda1=ifelse(pred_qda$posterior[,2]>=qda_threshold,'yes','no')
table(pred_qda1,test$testy)

#--------------------------Naive Bayes做預測------------------------
fit_nb=naiveBayes(train$trainy~.,data=train[,c(1,lassomodel)])
pred_nb2=predict(fit_nb,test)
table(pred_nb2,test$testy)
pred_nb = predict(fit_nb, newdata = test, type="raw")
pred_nb = pred_nb[, 2]
#ROC curve for Naive Bayes
roc_nb=roc(test$testy,pred_nb,plot=T,auc=T)
roc_nb$auc
title(main='ROC Curve for Naive Bayes method (AUC=0.7563)')
#Youden's Index to choose cut point
nb_threshold=roc_nb$thresholds[which.max(roc_nb$sensitivities+
                                           roc_nb$specificities-1)]
nb_threshold
pred_nb1=rep(NA,nrow(test))
pred_nb1=ifelse(pred_nb>=nb_threshold,'yes','no')
table(pred_nb1,test$testy)


