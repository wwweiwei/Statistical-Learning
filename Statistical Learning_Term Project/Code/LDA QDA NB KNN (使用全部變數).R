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

#--------------------------all variables-----------------------------
#--------------------------LDA做預測------------------------------------
fit_lda0=lda(trainy~.,data=train)
pred_lda0=predict(fit_lda,test)
table(pred_lda0$class,test$testy)
#ROC curve for LDA
roc_lda0=roc(test$testy, pred_lda0$posterior[,2], plot = T,auc=T)
roc_lda0$auc
title(main='ROC Curve for LDA (AUC=0.7552)')
#Youden's Index to choose cut point
lda_threshold0=roc_lda0$thresholds[which.max(roc_lda0$sensitivities+
                                               roc_lda0$specificities-1)]
lda_threshold0
pred_lda10=rep(NA,nrow(test))
pred_lda10=ifelse(pred_lda0$posterior[,2]>=lda_threshold0,'yes','no')
table(pred_lda10,test$testy)

#--------------------------QDA做預測------------------------------------
fit_qda0=qda(trainy~.,data=train)
#rank defficiency 因為解釋變數太多線性相依

#--------------------------Naive Bayes做預測------------------------
fit_nb0=naiveBayes(train$trainy~.,data=train)
pred_nb20=predict(fit_nb,test)
table(pred_nb20,test$testy)
pred_nb0 = predict(fit_nb0, newdata = test, type="raw")
pred_nb0 = pred_nb0[, 2]
#ROC curve for Naive Bayes
roc_nb0=roc(test$testy,pred_nb0,plot=T,auc=T)
roc_nb0$auc
title(main='ROC Curve for Naive Bayes method (AUC=0.7589)')
#Youden's Index to choose cut point
nb_threshold0=roc_nb0$thresholds[which.max(roc_nb0$sensitivities+
                                             roc_nb0$specificities-1)]
nb_threshold0
pred_nb10=rep(NA,nrow(test))
pred_nb10=ifelse(pred_nb0>=nb_threshold0,'yes','no')
table(pred_nb10,test$testy)


#--------------------------KNN N=5做預測--------------------------
pred_knn= knn(train[,-1], test[,-1],
              train$trainy, k=5,prob=T)
table(pred_knn,test$testy)
#ROC curve for KNN k=5
roc_knn5=roc(test$testy,attr(pred_knn,"prob"),plot=T,auc=T)
roc_knn5$thresholds[which.max(roc_knn5$sensitivities+roc_knn5$specificities-1)]
roc_knn5$auc
title(main='ROC Curve for KNN k=5 (AUC=0.6425)')
#Youden's Index to choose cut point
knn5_threshold=roc_knn5$thresholds[which.max(roc_knn5$sensitivities+
                                               roc_knn5$specificities-1)]
knn5_threshold
pred_knn51=rep(NA,nrow(test))
pred_knn51=ifelse(attr(pred_knn,"prob")<=knn5_threshold,'yes','no')
table(pred_knn51,test$testy)

#--------------------------KNN N=10做預測--------------------------
set.seed(1)
pred_knn2= knn(train[,-1], test[,-1],
               train$trainy, k=10,prob=T)
table(pred_knn2,test$testy)
#ROC curve for KNN k=10
roc_knn10=roc(test$testy,attr(pred_knn2,"prob"),plot=T,auc=T)
roc_knn10$thresholds[which.max(roc_knn10$sensitivities+roc_knn10$specificities-1)]
roc_knn10$auc
title(main='ROC Curve for KNN k=10 (AUC=0.6847)')
#Youden's Index to choose cut point
knn10_threshold=roc_knn10$thresholds[which.max(roc_knn10$sensitivities+
                                                 roc_knn10$specificities-1)]
knn10_threshold
pred_knn101=rep(NA,nrow(test))
pred_knn101=ifelse(attr(pred_knn2,"prob")<=knn10_threshold,'yes','no')
table(pred_knn101,test$testy)

#--------------------------KNN N=20做預測--------------------------
set.seed(1)
pred_knn3= knn(train[,-1], test[,-1],
               train$trainy, k=20,prob=T)
table(pred_knn3,test$testy)
#ROC curve for KNN k=20
roc_knn20=roc(test$testy,attr(pred_knn3,"prob"),plot=T,auc=T)
roc_knn20$auc
title(main='ROC Curve for KNN k=20 (AUC=0.701)')
#Youden's Index to choose cut point
knn20_threshold=roc_knn20$thresholds[which.max(roc_knn20$sensitivities+
                                                 roc_knn20$specificities-1)]
knn20_threshold
pred_knn201=rep(NA,nrow(test))
pred_knn201=ifelse(attr(pred_knn3,"prob")<=knn20_threshold,'yes','no')
table(pred_knn201,test$testy)


