# HW4
# 10.
install.packages("ISLR")
library(ISLR)
library(stats)
install.packages("MLmetrics")
library(MLmetrics)
install.packages("e1071")
library(e1071)
install.packages("ISLR")
library(ISLR)
install.packages("MASS")
library(MASS)
install.packages("class")
library(class)
install.packages("tidyverse")
library(tidyverse)
data(Weekly)
head(Weekly,10)

#(a)
?Weekly
summary(Weekly)
dim(Weekly)
str(Weekly)
plot(x=Weekly$Year,y=Weekly$Volume,main="Year to Volume",xlab="Year",ylab="Volume(billions)")
plot(x=Weekly$Year,y=Weekly$Today,main="Year to Today",xlab="Year",ylab="Today(%return for the week)")

# (b)
fit_glm <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(fit_glm)

# (c)
dir = predict(fit_glm, type="response")
dir
pred = rep("Up", 1089)
pred[dir < 0.5] = "Down"
table(pred, Weekly$Direction)
# Accuracy(y_pred = pred, y_true = Weekly$Direction)
# Recall(y_pred = pred, y_true = Weekly$Direction)
# Precision(y_pred = pred, y_true = Weekly$Direction)

# (d)
train <- (Weekly$Year < 2009)
train
Weekly_1990_2008 <- Weekly[train, ]
Weekly_2009_2010 <- Weekly[!train, ]
# Weekly_1990_2008
# Weekly_2009_2010
Direction_1990_2008 <- Weekly$Direction[train]
Direction_2009_2010 <- Weekly$Direction[!train]

# Direction_1990_2008
length(Direction_1990_2008)
lag2_glm <- glm(Direction ~ Lag2, data=Weekly_1990_2008, family=binomial)
summary(lag2_glm)

pred_glm <- predict(lag2_glm, type="response", newdata = Weekly_2009_2010)
plot(pred_glm, pch= ifelse(Weekly_2009_2010$Direction=="Down",8,16), main="predicted direction of 2009 to 2010")
abline(h = 0.5, lwd= 1)

pred2 = rep("Up", 104)
pred2[pred_glm < 0.5] = "Down"
pred2
table(pred2,Direction_2009_2010)

# (e)
lag2_lda <- lda(Direction ~ Lag2, data=Weekly_1990_2008, family=binomial)
lag2_lda

lda.pred = predict(lag2_lda,Weekly_2009_2010)
table(lda.pred$class,Weekly_2009_2010$Direction)

# (f)
lag2_qda <- qda(Direction ~ Lag2, data=Weekly_1990_2008, family=binomial)
lag2_qda

qda.pred = predict(lag2_qda,Weekly_2009_2010)
table(qda.pred$class,Weekly_2009_2010$Direction)

# (g)
train_X <- cbind(Weekly_1990_2008$Lag2)
test_X <- cbind(Weekly_2009_2010$Lag2)
train_Y <- cbind(Weekly_1990_2008$Direction)
set.seed(1)
# predict when k=1
pred_knn_1 <- knn(train_X, test_X, train_Y, k=1)
table(pred_knn_1, Weekly_2009_2010$Direction)

## Naive Bayes
fit_nb <- naiveBayes(Direction ~ Lag2, data=Weekly, subset=train)
fit_nb

# predict Direction, result : class
pred_nb <- predict(fit_nb, Weekly_2009_2010)
table(pred_nb)
table(pred_nb, Weekly_2009_2010$Direction)
mean(pred_nb == Weekly_2009_2010$Direction)

# (i)
## Logistic Regression
lag2_glm <- glm(Direction ~ Lag2+Lag3+Lag4+Lag5, data=Weekly_1990_2008, family=binomial)
summary(lag2_glm)
pred_glm <- predict(lag2_glm, type="response", newdata = Weekly_2009_2010)
plot(pred_glm, pch= ifelse(Weekly_2009_2010$Direction=="Down",8,16), main="predicted direction of 2009 to 2010")
abline(h = 0.5, lwd= 1)
pred2 = rep("Up", 104)
pred2[pred_glm < 0.5] = "Down"
pred2
table(pred2,Direction_2009_2010)
## LDA
lag2_lda <- lda(Direction ~ Lag2+Lag3, data=Weekly_1990_2008, family=binomial)
lag2_lda
lda.pred = predict(lag2_lda,Weekly_2009_2010)
table(lda.pred$class,Weekly_2009_2010$Direction)
## QDA
lag2_qda <- qda(Direction ~ Lag2+Lag3, data=Weekly_1990_2008, family=binomial)
lag2_qda
qda.pred = predict(lag2_qda,Weekly_2009_2010)
table(qda.pred$class,Weekly_2009_2010$Direction)
## KNN K=2
train_X <- cbind(Weekly_1990_2008$Lag2)
test_X <- cbind(Weekly_2009_2010$Lag2)
train_Y <- cbind(Weekly_1990_2008$Direction)
set.seed(1)
pred_knn_1 <- knn(train_X, test_X, train_Y, k=4)
table(pred_knn_1, Weekly_2009_2010$Direction)
## NaiveBayes
fit_nb <- naiveBayes(Direction ~ Lag3+Lag4, data=Weekly, subset=train)
fit_nb
# predict Direction, result : class
pred_nb <- predict(fit_nb, Weekly_2009_2010)
table(pred_nb)
table(pred_nb, Weekly_2009_2010$Direction)
mean(pred_nb == Weekly_2009_2010$Direction)

# 11.
?Auto
summary(Auto)
dim(Auto)
str(Auto)
#(a)
mpg_median = median(Auto$mpg)
mpg_median
Auto$mpg01 = rep(0,392)
Auto$mpg01[Auto$mpg > mpg_median] = 1
head(Auto)
summary(Auto$mpg01)
# (b)
cor(Auto$mpg01,na.omit(Auto[-9]))
#pairs(Auto)
plot(Auto$mpg01, Auto$cylinders,main="mpg01 v.s cylinders",pch=16,xlab = "mpg01",ylab = "cylinders")
plot(Auto$mpg01, Auto$displacement,main="mpg01 v.s displacement",pch=16,xlab = "mpg01",ylab = "displacement")
plot(Auto$mpg01, Auto$horsepower,main="mpg01 v.s horsepower",pch=16,xlab = "mpg01",ylab = "horsepower")
plot(Auto$mpg01, Auto$weight,main="mpg01 v.s weight",pch=16,xlab = "mpg01",ylab = "weight")
plot(Auto$mpg01, Auto$acceleration,main="mpg01 v.s acceleration",pch=16,xlab = "mpg01",ylab = "acceleration")
plot(Auto$mpg01, Auto$year,main="mpg01 v.s year",pch=16,xlab = "mpg01",ylab = "year")
plot(Auto$mpg01, Auto$origin,main="mpg01 v.s origin",pch=16,xlab = "mpg01",ylab = "origin")
# (c)
id <- sample(1:dim(Auto)[1], size=dim(Auto)[1]*0.75)
training <- Auto[id,]
test <- Auto[-id,]
# (d)
# (e)
# (f)
# (g)
