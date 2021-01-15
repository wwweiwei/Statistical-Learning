### Statistical Learning Term Project
### Deposit Subscription Prediction

* Dataset:UCI Machine Learning Repository
The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution.
* numbers of data:41888 
* numbers of data columns:21

* Purpose:
  * The classification goal is to predict if the client will subscribe (yes/no) a term deposit (variable y).
  * Indicator: Recall (TP/TP+FN) → F1 → AUC
  
* Methods: 
  * Preprocessing: 
    * MICE:用 CART 的方法模擬遺漏值後，再進行填補
    * SMOTE:使用 Synthesized Minority Oversampling Technique (SMOTE) 增加Yes的資料，原理是在少數樣本位置近的地方，人工合成一些樣本
  * Model:
    * Adaptive LASSO
    * Simple linear regression
    * LASSO Regression
    * LASSO with Adaptive LASSO
    * RIDGE Regression
    * Logistic Regression
    * Linear discriminant analysis
    * Quadratic discriminant analysis 
    * Naive Bayes
    * k-nearest neighbor 
    * Classification tree 
    * Bagging
    * Random forest
    * Boosting 
    * XGBoost 
    * SVM
    
  * Conclusion
    * Recall → F1 → AUC
    * 大部分的模型:有進行 model selection 的預測結果 (LASSO或Adaptive LASSO) 通常比直接將所有變數納入考慮的模型來得好。
    * 從vip()函數可發現,對結果影響最大的變數是 僱員人數 (nr.employed) 及 消費者信心指數(cons.conf.idx),而這兩個變數有都有在LASSO及adaptive LASSO所選取的變數中 
    * 線性模型:使用 Adaptive LASSO 所選變數建立的 Naïve Bayes 模型表現最好
    * tree模型:XGBoost 建立的模型表現最好
