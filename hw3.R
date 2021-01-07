# HW3-106070038
## 1.
library(glmnet)
set.seed(36)
n<-100
sigma <- 5
beta0 <- c(2,-2,0.5,1,-3)
cormat <- diag(1,nrow=5,ncol=5)
cormat[cormat==0] <- 0.5
cholmat <- chol(cormat)
x <- matrix(rnorm(5*n,0,1), ncol=5)%*%cholmat
err <- rnorm(n,0,sigma)
y <- x%*%beta0 +err

## 2.
## (2a)
beta_hat = 0 #init
for (i in 100) {
    y_x_beta = y[i] - x[i,1]*2 + x[i,2]*2 - x[i,3]*0.5 - x[i,4]*1 + x[i,5]*3
    square = y_x_beta^2
    beta_hat = beta_hat + square
}
beta_hat #print

### scale x and y
scale_x = scale(x) 
scale_y = y - mean(y)

beta_tilde = 0 #init
for (i in 100) {
  y_x_beta = scale_y[i] - scale_x[i,1]*2 + scale_x[i,2]*2 - scale_x[i,3]*0.5 - scale_x[i,4]*1 + scale_x[i,5]*3
  square = y_x_beta^2
  beta_tilde = beta_tilde + square
}
beta_tilde #print

## (2b)
lambda_tilde_beta <- matrix( nrow = 16, ncol = 2) 
lambda <- c(2^5,2^4,2^3,2^2,2^1,2^0,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9,2^-10)
lambda
lambda_tilde_beta[,1]<- c(2^5,2^4,2^3,2^2,2^1,2^0,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9,2^-10)
#lambda_tilde_beta
beta_square = 2^2+(-2)^2+0.5^2+1^2+(-3)^2
y_xbeta = 0
for (i in 100) {
  square = (scale_y[i] - scale_x[i,1]*2 + scale_x[i,2]*2 - scale_x[i,3]*0.5 - scale_x[i,4]*1 + scale_x[i,5]*3)^2
  y_xbeta = y_xbeta + square
}
buf = 2*n
buf
y_xbeta
y_xbeta = y_xbeta/buf
for (j in c(1:16)) {
  lambda_tilde_beta[j,2] = y_xbeta + lambda[j]*beta_square
}
lambda_tilde_beta
graphics.off()
plot(log(lambda_tilde_beta[,1]), lambda_tilde_beta[,2], xlab="log(lambda)",
     ylab="tilde_beta(lambda)",main="2(b) Plot")

## beta_hat_2
lambda = 2
lambda_hat_beta <- matrix( nrow = 1, ncol = 2) 
lambda_hat_beta[1]<- 2
#lambda_tilde_beta
beta_square = 2^2+(-2)^2+0.5^2+1^2+(-3)^2
y_xbeta = 0
for (i in 100) {
  square = (y[i] - x[i,1]*2 + x[i,2]*2 - x[i,3]*0.5 - x[i,4]*1 + x[i,5]*3)^2
  y_xbeta = y_xbeta + square
}
y_xbeta = y_xbeta/(2*n)
lambda_hat_beta[2] = y_xbeta + lambda*beta_square
lambda_hat_beta

## (2c)
CVRIDGE = cv.glmnet(scale_x,scale_y,family = "gaussian",type.measure ='mse',nfold = 5,alpha = 0,lambda =c(2^5,2^4,2^3,2^2,2^1,2^0,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9,2^-10))
CVRIDGE
plot(CVRIDGE)
# Use lambda.min
ridge1 = glmnet(x,y,family = "gaussian",alpha = 0,lambda = CVRIDGE$lambda.min)   # lambda代lambda.min
ridge1$beta   #係數

# Use lambda.1se
ridge2 = glmnet(x,y,family = "gaussian",alpha = 0,lambda = CVRIDGE$lambda.1se)   # lambda代lambda.1se
ridge2$beta   #係數

## (2d)
lambda_hat_beta <- matrix( nrow = 16, ncol = 2) 
lambda <- c(2^5,2^4,2^3,2^2,2^1,2^0,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9,2^-10)
lambda_hat_beta[,1]<- c(2^5,2^4,2^3,2^2,2^1,2^0,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9,2^-10)
beta_square = 2^2+(-2)^2+0.5^2+1^2+(-3)^2
y_xbeta = 0
for (i in 100) {
  square = (y[i] - x[i,1]*2 + x[i,2]*2 - x[i,3]*0.5 - x[i,4]*1 + x[i,5]*3)^2
  y_xbeta = y_xbeta + square
}
y_xbeta = y_xbeta/(2*n)
for (j in c(1:16)) {
  lambda_hat_beta[j,2] = y_xbeta + lambda[j]*beta_square
}
lambda_hat_beta
graphics.off()
plot(log(lambda_hat_beta[,1]), lambda_hat_beta[,2], xlab="log(lambda)",
     ylab="beta_hat(lambda)",main="2(d) Plot")