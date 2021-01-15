library(mice)
library(gbm)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(DMwR)
library(pROC)


bank = read.table("bank-additional-full.csv",header=TRUE,sep=";")
str(bank)
head(bank)
summary(bank)

bank = bank[,-11]
for (i in 2:10) {
  bank[,i] = as.factor(bank[,i])
}
bank[,14] = as.factor(bank[,14])
bank[,20] = as.factor(bank[,20])
for (i in 1:nrow(bank)) {
  for (j in 1:ncol(bank)) {
    if(bank[i,j]=="unknown") bank[i,j] = NA
  }
}
mice.bank <- mice(bank,
                  m = 5,           # 產生五個被填補好的資料表
                  maxit = 5,      # max iteration
                  method = "cart", # 使用CART決策樹，進行遺漏值預測
                  seed = 3)      # set.seed()，令抽樣每次都一樣
complete(mice.bank, 1) # 1st data
complete(mice.bank, 2) # 2nd data
complete(mice.bank, 3) # 3rd data
complete(mice.bank, 4) # 4th data
complete(mice.bank, 5) # 5th data

banknew = complete(mice.bank,3)



banknew[,1] = as.integer(banknew[,1])
for (i in 2:10) {
  banknew[,i] = as.factor(banknew[,i])
}
banknew[,14] = as.factor(banknew[,14])
banknew[,20] = as.factor(banknew[,20])

for (i in 1:nrow(banknew)) {
  if(banknew[i,12]==999) banknew[i,12] = NA
}

set.seed(1)
index = sample(1:nrow(banknew),0.8*ceiling(nrow(banknew)))
train = banknew[index,]
test = banknew[-index,]
for (i in 1:nrow(test)) {
  if(is.na(test[i,12])) test[i,12] = 999
}

class(train)
str(train)
table(train$y)



set.seed(1)
train_smote = SMOTE(y ~ .,train , perc.over = 600,perc.under = 100)
for (i in 1:nrow(train_smote)) {
  if(is.na(train_smote[i,12])) train_smote[i,12] = 999
}
train_smote[,1] = as.integer(train_smote[,1])
train_smote[,11] = as.integer(train_smote[,11])
train_smote[,12] = as.integer(train_smote[,12])
train_smote[,13] = as.integer(train_smote[,13])


table(train_smote$y)
str(train_smote)
str(test)

test[,1] = as.integer(test[,1])
test[,11] = as.integer(test[,11])
test[,12] = as.integer(test[,12])
test[,13] = as.integer(test[,13])



#classification tree with rpart
bank_tree = rpart(y~.,data = train_smote)
bank_tree
rpart.plot(bank_tree)

pred = predict(bank_tree,newdata = test,type = "class")
table(pred,test$y)
mean(pred == test$y) #0.8770333


#用PROC畫圖
modelroc <- roc(test$y,pred)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)


#classification tree
bank_tree2 = tree(y~.,data = train_smote)
summary(bank_tree2)
bank_tree2
plot(bank_tree2);text(bank_tree2,pretty=0)
pred1 = predict(bank_tree2,newdata = test,type = "class")
table(pred1,test$y)
mean(pred1 == test$y)

# 利用cross-validation找最佳樹的大小
set.seed(1)
cv_bank_tree2 = cv.tree(bank_tree2, FUN = prune.misclass)
cv_bank_tree2
plot(cv_bank_tree2$size ,cv_bank_tree2$dev ,type="b")

prune_tree = prune.tree(bank_tree2,best = 6)  #修剪
plot(prune_tree)
text(prune_tree,pretty=0) 


summary(bank_tree)
summary(bank_tree2)
summary(prune_tree)
prune_tree


pred = predict(bank_tree,newdata = test,type = "class")
table(pred,test$y)
pred1 = predict(bank_tree2,newdata = test,type = "class")
table(pred1,test$y)
pred2 = predict(prune_tree,newdata = test, type = "class")  #利用修剪完的tree對test data做預測
table(pred2,test$y)
mean(pred2 == test$y) #0.7226269


pred2 = as.numeric(pred2)
#用PROC畫圖
modelroc <- roc(test$y,pred2)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

#Bagging

set.seed(1)
bag.bank = randomForest(y~. , data = train_smote, mtry = 19, importance=T)  

bag.bank
pred4 = predict(bag.bank,newdata = test)

table(pred4,test$y)
mean(pred4 == test$y) #0.8770333

importance(bag.bank)
varImpPlot(bag.bank,main="variable importance plot")

pred4 = as.numeric(pred4)
#用PROC畫圖
modelroc <- roc(test$y,pred4)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)


#Ranfom Forest
set.seed(1)
rf.bank = randomForest(y~. , data = train_smote, mtry = 4, importance=T) 
rf.bank

pred5 = predict(rf.bank,newdata = test)
table(pred5,test$y)
mean(pred5 == test$y) #0.8905074

importance(rf.bank)
varImpPlot(rf.bank)

pred5 = as.numeric(pred5)
#用PROC畫圖
modelroc <- roc(test$y,pred5)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

tuneRF(x=train_smote[,-20],y=train_smote[,20],stepFactor=0.5)

set.seed(1)
rf.bank.revise = randomForest(y~. , data = train_smote, mtry = 8, importance=T) 
rf.bank.revise

pred6 = predict(rf.bank.revise,newdata = test)
table(pred6,test$y)
mean(pred6 == test$y)

pred6 = as.numeric(pred6)
#用PROC畫圖
modelroc <- roc(test$y,pred6)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)


#Boosting
str(train_smote)
str(test)

for (i in 2:10) {
  train_smote[,i] = as.numeric(train_smote[,i]) 
}
train_smote[,14] = as.numeric(train_smote[,14])
train_smote[,20] = as.numeric(train_smote[,20])

for (i in 2:10) {
  test[,i] = as.numeric(test[,i]) 
}
test[,14] = as.numeric(test[,14])
test[,20] = as.numeric(test[,20])
train_smote[,20] = train_smote[,20]-1
test[,20] = test[,20]-1

set.seed(1)
pows <- seq(-10, -1, by = 0.1)
lambdas <- 10^pows
train_smote.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.bank <- gbm(y ~ ., data = train_smote, distribution = "bernoulli", n.trees = 1000, shrinkage = lambdas[i])
  boost.probs <- predict(boost.bank, train_smote, n.trees = 1000)
  boost.pred.train <- ifelse(boost.probs > 0.5, 1, 0)
  train_smote.err[i] <- mean(boost.pred.train != train_smote$y)
}

plot(lambdas, train_smote.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

set.seed(1)
test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.bank <- gbm(y ~ ., data = train_smote, distribution = "bernoulli", n.trees = 1000, shrinkage = lambdas[i])
  boost.probs <- predict(boost.bank, test, n.trees = 1000)
  boost.pred.test <- ifelse(boost.probs > 0.5, 1, 0)
  test.err[i] <- mean(boost.pred.test != test$y)
}

plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Testing MSE")

set.seed(1)
boost.bank <- gbm(y ~ ., data = train_smote, distribution = "bernoulli", n.trees = 1000, shrinkage = lambdas[91])
boost.probs <- predict(boost.bank, test, n.trees = 1000)
boost.pred.test <- ifelse(boost.probs > -1.062, 1, 0)
table(boost.pred.test, test$y)
mean(boost.pred.test == test$y)

boost.bank
summary(boost.bank)
vip::vip(boost.bank)

#用PROC畫圖
modelroc <- roc(test$y,boost.probs)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
