library(xgboost)
library(vip)

summary(banknew)
str(banknew)

# 1. 將資料格式(Data.frame)，用"xgb.DMatrix()" 轉換為 xgboost 的稀疏矩

dtrain = xgb.DMatrix(data = as.matrix(train_smote[,1:19]),
                     label = train_smote$y)
dtest = xgb.DMatrix(data = as.matrix(test[,1:19]),
                    label = test$y)



str(train_smote)

# 2. 設定xgb.params，也就是 xgboost 裡面的參數

xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.8,                    
  # row的抽樣比例，越高表示每棵樹使用的row越多，會增加每棵小樹的複雜度
  subsample = 0.8,                      
  booster = "gbtree",
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 5,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.1,
  # 或用'mae'也可以
  #eval_metric = "error",                      
  objective = "binary:logistic",
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0)               

cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=200,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20, # 每20個單位才顯示一次結果
  seed = 1
) 
tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="error", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )
# 獲得 best nround
best.nrounds = cv.model$best_iteration 
best.nrounds

# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

# 如果要畫出 xgb 內的所有決策樹，可以用以下函式(但因為會很多，這裡就不畫了)
# xgb.plot.tree(model = xgb.model) 

# 預測
xgb_y = predict(xgb.model, dtest)
xgb_pred_test <- ifelse(xgb_y >0.175, 1, 0)
table(xgb_pred_test,test$y)
mean(xgb_pred_test == test$y) # MSE

xgb.model

#用PROC畫圖
modelroc <- roc(test$y,xgb_y )
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)



vip::vip(xgb.model)

510/937
510/(735+510)
6566/(8238-937)
