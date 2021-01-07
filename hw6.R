## 4.
# (b)
plot( xlim = c(-2, 2), ylim = c(-3, 3), xlab = "X1", ylab = "X2")
lines(x = c(-2, 2), y = c(1, 1))

lines(x = c(1, 1), y = c(-3, 1))
text(x = (-2 + 1)/2, y = -1, labels = c(-1.8))
text(x = 1.5, y = -1, labels = c(0.63))

lines(x = c(-2, 2), y = c(2, 2))
text(x = 0, y = 2.5, labels = c(2.49))

lines(x = c(0, 0), y = c(1, 2))
text(x = -1, y = 1.5, labels = c(-1.06))
text(x = 1, y = 1.5, labels = c(0.21))

## 8.
## (a)
data(Carseats, package="ISLR")
str(Carseats)
head(Carseats)
summary(Carseats)
set.seed(5)
index = sample(1:nrow(Carseats),ceiling(0.8*nrow(Carseats)))
train = Carseats[index,]
test = Carseats[-index,]
str(train)
str(test)
## (b)
library(rpart)
tree = rpart(Sales~.,data=train)  
tree
summary(tree)
rpart.plot(tree)  #視覺化
pred = predict(tree,newdata = test)  #預測test data
mean((pred - test$Sales)^2)  # 計算MSE

# cp
tree = rpart(Sales~., data=train, cp=0.03) 
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2)  

tree = rpart(Sales~., data=train, cp=0.05)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

tree = rpart(Sales~., data=train, cp=0.01)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 
# minbucket
tree = rpart(Sales~., data=train, minbucket=1)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

tree = rpart(Sales~., data=train, minbucket=5)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

tree = rpart(Sales~., data=train, minbucket=10)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

tree = rpart(Sales~., data=train, minbucket=15)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

tree = rpart(Sales~., data=train, minbucket=20)  
pred = predict(tree,newdata = test)
rpart.plot(tree)
mean((pred - test$Sales)^2) 

## (c)
library(tree)
tree2 = tree(Sales~.,data=train)
tree2
plot(tree2) ;  text(tree2,pretty=0)

# 利用未修剪的tree對test data做預測
pred1 = predict(tree2,newdata = test)
mean((pred1 - test$Sales)^2)  # 計算MSE
# 利用cross-validation找最佳樹的大小
set.seed(10)
cv_tree = cv.tree(tree2,K=5)  # K-folds CV
cv_tree  
plot(cv_tree$size ,cv_tree$dev ,type="b")
tree.min <- which.min(cv_tree$dev)
tree.min
points(tree.min, cv_tree$dev[tree.min],cex = 1.5, pch = 16)
# choose best size = 6

prune_tree = prune.tree(tree2,best = 6)  #修剪
plot(prune_tree)
text(prune_tree,pretty=0)

pred2 = predict(prune_tree,newdata = test)  #利用修剪完的tree對test data做預測
mean((pred2 - test$Sales)^2)  # 計算MSE

## (d) bagging
B = 100  # 抽100次boostrap sample
B_pred = matrix(ncol=B,nrow=nrow(test))
for (i in 1:B){
  B_sample = train[sample(1:nrow(train),replace = T),]   # bootstrap sample
  B_tree = rpart(Sales~.,data=B_sample)
  B_pred[,i] = predict(B_tree,newdata=test)
}
mean((apply(B_pred,1,mean) - test$Sales)^2)  # 取100次平均 計算MSE
B_tree$variable.importance

## (e) RF
set.seed(10)
rf = randomForest(Sales~.,data=train)
rf
plot(rf)
pred3 = predict(rf,newdata = test)
mean((pred3 - test$Sales)^2) # 計算MSE
importance(rf)
varImpPlot(rf,main="variable importance plot")

rf = randomForest(Sales~., mtry = 4 ,data=train)
plot(rf)
pred3 = predict(rf,newdata = test)
mean((pred3 - test$Sales)^2) 

rf = randomForest(Sales~., mtry = 6 ,data=train)
plot(rf)
pred3 = predict(rf,newdata = test)
mean((pred3 - test$Sales)^2)

rf = randomForest(Sales~., mtry = 10 ,data=train)
plot(rf)
pred3 = predict(rf,newdata = test)
mean((pred3 - test$Sales)^2)

## 9.
##(a)
set.seed(1)
data(OJ, package="ISLR")
str(OJ)
head(OJ)
summary(OJ)

index = sample(1:nrow(OJ), 800)
train = OJ[index,]
test = OJ[-index,]
str(train)
str(test)
## (b)
# library(rpart)
# tree = rpart(Purchase~.,data=train)  

tree2 = tree(Purchase~.,data=train)
tree2
plot(tree2) ;  text(tree2,pretty=0)
pred = predict(tree2,newdata = test)  #預測test data

error = 0
for (i in c(1:270)) {
  brand = NULL
  if (pred[i,1]>pred[i,2])
    brand = 'CH'
  else
    brand = 'MM'
  if (brand!=test[i,]$Purchase) {
    error=error+1
  }
}
error_rate = error/270
error_rate


## (c)
tree2
## (d)
plot(tree2) ;  text(tree2,pretty=0)
## (e)
pred = predict(tree2,newdata = test)  #預測test data
pred_list=NULL
for (i in c(1:270)) {
  brand = NULL
  if (pred[i,1]>pred[i,2])
    brand = 'CH'
  else
    brand = 'MM'
  pred_list[i]=brand
}
table(pred_list, test$Purchase)
## (f)
set.seed(3)
cv.oj <- cv.tree(tree2, FUN = prune.misclass)
cv.oj

## (g)
plot(cv.oj$size, cv.oj$dev / nrow(train), type = "b",
     xlab = "Tree Size", ylab = "CV classification error rate")
cv.oj$dev/ nrow(train)
## (h)
min_idx = which.min(cv.oj$dev)
min_idx
cv.oj$size[min_idx]
## (i)
prune_tree = prune.tree(tree2,best = 7)  #修剪
plot(prune_tree)
text(prune_tree,pretty=0)
summary(prune_tree)
## (j)
### unpruned
unprune_trn_pred = predict(tree2, train, type = "class")
table(predicted = unprune_trn_pred, actual = train$Purchase)
### pruned
prune_trn_pred = predict(prune_tree, train, type = "class")
table(predicted = prune_trn_pred, actual = train$Purchase)
## (k)
### unpruned
unprune_tst_pred = predict(tree2, test, type = "class")
table(predicted = unprune_tst_pred, actual = test$Purchase)
### pruned
prune_tst_pred = predict(prune_tree, test, type = "class")
table(predicted = prune_tst_pred, actual = test$Purchase)

## 10.
## (a)
data(Hitters, package="ISLR")
str(Hitters)
head(Hitters)
summary(Hitters)

Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)
## (b)

index = sample(1:nrow(Hitters), 200)
train = Hitters[index,]
test = Hitters[-index,]
str(train)
str(test)

## (c) boosting
#install.packages("gbm")
library(gbm)

set.seed(1)

exp_seq <- seq(-10, -0.2, by = 0.05)
lambda <- 10^exp_seq
train.mse <- rep(NA, length(lambda))
for (i in 1:length(lambda)) {
  boost.hitters <- gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred.train <- predict(boost.hitters, train, n.trees = 1000)
  train.mse[i] <- mean((pred.train - train$Salary)^2)
}
plot(lambdas, train.mse, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")
train.mse
## (d)
test.mse <- rep(NA, length(lambda))
for (i in 1:length(lambda)) {
  boost.hitters <- gbm(Salary ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  yhat <- predict(boost.hitters, test, n.trees = 1000)
  test.mse[i] <- mean((yhat - test$Salary)^2)
}
plot(lambda, test.mse, type = "b", xlab = "Shrinkage values", ylab = "Testing MSE")
test.mse
min(test.mse)
lambda[which.min(test.mse)]
## (e)
### linear regression
library(glmnet)
fit <- lm(Salary ~ ., data = train)
pred_lm <- predict(fit, test)
mean((pred_lm - test$Salary)^2)
### ridge regression
x <- model.matrix(Salary ~ ., data = train)
x.test <- model.matrix(Salary ~ ., data = test)
y <- train$Salary
fit2 <- glmnet(x, y, alpha = 0)
pred_rid <- predict(fit2, s = 0.01, newx = x.test)
mean((pred_rid - test$Salary)^2)
## (f)
boost.Salary = gbm(Salary~., data = train, distribution = "gaussian",
                   n.trees = 1000, interaction.depth = 4, shrinkage = lambda[which.min(test.mse)])
summary(boost.Salary)
## (g)
B = 100  # 抽100次boostrap sample
B_pred = matrix(ncol=B,nrow=nrow(test))
for (i in 1:B){
  B_sample = train[sample(1:nrow(train),replace = T),]  
  B_tree = rpart(Salary~.,data=B_sample)
  B_pred[,i] = predict(B_tree,newdata=test)
}
mean((apply(B_pred,1,mean) - test$Salary)^2)  # 取100次平均 計算MSE
B_tree$variable.importance
