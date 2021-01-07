# 8.
# (a)
set.seed(10)
X <- rnorm(100)
epsilon <- rnorm(100)
# (b)
b0 <- 4
b1 <- 3
b2 <- 2
b3 <- 1
Y <- b0+b1*X+b2*X^2+b3*X^3+epsilon
Y
# (c)
install.packages("leaps")
library(leaps)
data <- data.frame(y = Y, x = X)
regfit <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10)
regsum <- summary(regfit)
regsum
# ?formula
names(regsum)
# cp
plot(regsum$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(regsum$cp), reg.summary$cp[which.min(regsum$cp)], col = "red", cex = 2, pch = 15)
coef(regfit, which.min(regsum$cp))
# bic
plot(regsum$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(regsum$bic), regsum$bic[which.min(regsum$bic)], col = "red", cex = 2, pch = 15)
coef(regfit, which.min(regsum$bic))
# adjr2
plot(regsum$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regsum$adjr2), regsum$adjr2[which.max(regsum$adjr2)], col = "red", cex = 2, pch = 15)
coef(regfit, which.max(regsum$adjr2))

# (d)
# forward
regfit_fwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10, method = "forward")
regsum.fwd <- summary(regfit_fwd)
regsum.fwd
# Cp
plot(regsum.fwd$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(regsum.fwd$cp), regsum.fwd$cp[which.min(regsum.fwd$cp)], col = "red", cex = 2, pch = 17)
coef(regfit_fwd, which.min(regsum.fwd$cp))
# BIC
plot(regsum.fwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(regsum.fwd$bic), regsum.fwd$bic[which.min(regsum.fwd$bic)], col = "red", cex = 2, pch = 17)
coef(regfit_fwd, which.min(regsum.fwd$bic))
# adjr2
plot(regsum.fwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regsum.fwd$adjr2), regsum.fwd$adjr2[which.max(regsum.fwd$adjr2)], col = "red", cex = 2, pch = 17)
coef(regfit_fwd, which.max(regsum.fwd$adjr2))

# backward
regfit_bwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10, method = "backward")
regsum.bwd <- summary(regfit_bwd)
regsum.bwd
# Cp
plot(regsum.bwd$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(regsum.bwd$cp), regsum.bwd$cp[which.min(regsum.bwd$cp)], col = "red", cex = 2, pch = 15)
coef(regfit_bwd, which.min(regsum.bwd$cp))
# BIC
plot(regsum.bwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(regsum.bwd$bic), regsum.bwd$bic[which.min(regsum.bwd$bic)], col = "red", cex = 2, pch = 15)
coef(regfit_bwd, which.min(regsum.bwd$bic))
# adjr2
plot(regsum.bwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regsum.bwd$adjr2), regsum.bwd$adjr2[which.max(regsum.bwd$adjr2)], col = "red", cex = 2, pch = 15)
coef(regfit_bwd, which.max(regsum.bwd$adjr2))

# (e)
library(glmnet)
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)[, -1]
xmat
# lasso
cv <- cv.glmnet(xmat, Y, alpha = 1)
plot(cv)
names(cv)
cv$lambda.min
cv$lambda.1se
coef(cv, s = "lambda.min")

# (f)

b7 <- 7
Y <- b0 + b7 * X^7 + epsilon
Y
data <- data.frame(y = Y, x = X)
regfit <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, nvmax = 10)
regsum <- summary(regfit)
regsum
# cp
plot(regsum$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(regsum$cp), reg.summary$cp[which.min(regsum$cp)], col = "red", cex = 2, pch = 15)
coef(regfit, which.min(regsum$cp))
# bic
plot(regsum$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(regsum$bic), regsum$bic[which.min(regsum$bic)], col = "red", cex = 2, pch = 15)
coef(regfit, which.min(regsum$bic))
# adjr2
plot(regsum$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regsum$adjr2), regsum$adjr2[which.max(regsum$adjr2)], col = "red", cex = 2, pch = 15)
coef(regfit, which.max(regsum$adjr2))

# lasso
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)[, -1]
xmat
cv <- cv.glmnet(xmat, Y, alpha = 1)
plot(cv)
names(cv)
cv$lambda.min
cv$lambda.1se
coef(cv, s = "lambda.min")

# 10.
# (a)
set.seed(10)
x <- matrix(rnorm(1000 * 20), 1000, 20)
x
b <- rnorm(20)
b
b[4] <- 0
b[8] <- 0
b[12] <- 0
b[16] <- 0
b
epsilon <- rnorm(1000)
y <- x %*% b + epsilon
y
# (b)
set.seed(10)
train <- sample(seq(1000), 100, replace = FALSE)
test <- -train
x.train <- x[train, ]
x.test <- x[test, ]
y.train <- y[train]
y.test <- y[test]
# (c)
data.train <- data.frame(y = y.train, x = x.train)
regfit <- regsubsets(y ~ ., data = data.train, nvmax = 20)
regsum <- summary(regfit)

train.mat <- model.matrix(y ~ ., data = data.train, nvmax = 20)
val.errors_train <- rep(NA, 20)
for (i in 1:20) {
  # ?coef()
  coef <- coef(regfit, id = i)
  pred <- train.mat[, names(coef)] %*% coef
  val.errors_train[i] <- mean((pred - y.train)^2)
}
plot(val.errors_train, xlab = "Size", ylab = "Training MSE", pch = 15, type = "b")
val.errors_train

# (d)
data.test <- data.frame(y = y.test, x = x.test)
test.mat <- model.matrix(y ~ ., data = data.test, nvmax = 20)
val.errors_test <- rep(NA, 20)
for (i in 1:20) {
  coef <- coef(regfit, id = i)
  pred <- test.mat[, names(coef)] %*% coef
  val.errors_test[i] <- mean((pred - y.test)^2)
}
plot(val.errors_test, xlab = "Size", ylab = "Test MSE", pch = 15, type = "b")
val.errors_test

# (e)
which.min(val.errors_test)
# (f)
coef(regfit, which.min(val.errors_test))

# (g)
val.errors <- rep(NA, 20)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:20) {
  coefi <- coef(regfit, id = i)
  val.errors[i] <- sqrt(sum((b[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + sum(b[!(x_cols %in% names(coefi))])^2)
}
plot(val.errors, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 15, type = "b")
val.errors
min(val.errors)

