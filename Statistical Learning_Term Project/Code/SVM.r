## svm
library(e1071)

train = cbind(train_x,train_smote$y)
test = cbind(test_x,test$y)
colnames(train)[53] = "y" 
colnames(test)[53] = "y" 

## kernel="radial"
model = svm(formula = train$y ~ .,kernel="radial",  
            data = train)
train.pred = predict(model, train)
test.pred = predict(model, test)

# 訓練資料的混淆矩陣
table(real = train$y, predict = train.pred )
# 訓練資料的分類準確率
confus.matrix = table(real = train$y, predict = train.pred)
sum(diag(confus.matrix))/sum(confus.matrix) ##accuracy

# 測試資料的混淆矩陣
table(real = test$y, predict = test.pred)
# 測試資料的分類準確率
confus.matrix = table(real = test$y, predict = test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)

## kernel="polynomial"
model = svm(formula = train$y ~ .,kernel="polynomial",  
            data = train)
train.pred = predict(model, train)
test.pred = predict(model, test)

# 訓練資料的混淆矩陣
table(real = train$y, predict = train.pred )
# 訓練資料的分類準確率
confus.matrix = table(real = train$y, predict = train.pred)
sum(diag(confus.matrix))/sum(confus.matrix) 

# 測試資料的混淆矩陣
table(real = test$y, predict = test.pred)
# 測試資料的分類準確率
confus.matrix = table(real = test$y, predict = test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)

## kernel="sigmoid"
model = svm(formula = train$y ~ .,kernel="sigmoid",  
            data = train)
train.pred = predict(model, train)
test.pred = predict(model, test)

# 訓練資料的混淆矩陣
table(real = train$y, predict = train.pred )
# 訓練資料的分類準確率
confus.matrix = table(real = train$y, predict = train.pred)
sum(diag(confus.matrix))/sum(confus.matrix) 

# 測試資料的混淆矩陣
table(real = test$y, predict = test.pred)
# 測試資料的分類準確率
confus.matrix = table(real = test$y, predict = test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)