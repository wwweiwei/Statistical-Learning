# HW1 
## 106070038_wwDu
install.packages("glmnet")
install.packages("XQuartz")
library(glmnet)
library(datasets)
library(XQuartz) 
# 8.(a) 讀入檔案
college = read.csv("/Users/nkust/Desktop/2020Fall_Courses/Statistical Learning/College.csv", stringsAsFactors=FALSE)
print(college)
# 8.(b) 把每個row的名字設為university
rownames(college) = college[,1]
fix(college)
print("set the first column as each university")
print(college)
# 把第一個column刪掉
college = college[,-1]
fix(college)
print("eliminate the first column")
print(college)
# 8.(c) i
summary(college)
# 8.(c) ii
college$Private = as.factor(college$Private)
pairs(college[,1:10])
# 8.(c) iii
boxplot(college$Outstate ~ college$Private, col="orange", main="Side-by-side boxplots of Outstate versus Private", ylab="Outstate", xlab="Private") 
# 8.(c) iv
Elite=rep("No",nrow(college))
Elite[college$Top10perc >50]="Yes"
as.factor(Elite)
college=data.frame(college,Elite)
# there are 78 elite universities
summary(Elite)
summary(college)
boxplot(college$Outstate ~ college$Elite, col="orange", main="Side-by-side boxplots of Outstate versus Elite", ylab="Outstate", xlab="Elite") 
# 8.(c) v
par(mfrow=c(2,2))
hist(college$Books,main="Books ",xlab="Estimated book costs", breaks = 9)
hist(college$Top10perc,main="Top10perc",xlab="New students from top 10 % of high school class", breaks = 5)
hist(college$S.F.Ratio,main="S.F.Ratio",xlab="Student/faculty ratio")
hist(college$PhD,main="PhD",xlab="Percent of faculty with Ph.D.’s")
# 8.(c) vi
## Calculate the accept rate
Enroll_rate=rep(0,nrow(college))
for(i in 1:777) {
  Enroll_rate[i] = college[i,4]/college[i,2]
}
summary(Enroll_rate)
par(mfrow=c(1,1))
college=data.frame(college,Enroll_rate)
summary(college)
hist(college$Enroll_rate,main="Enroll rate",xlab="Enroll rate (Enroll divided by Apps)")
## Calculate the correlation between Enroll_rate and every columns
cor(college$Enroll_rate,college$Top10perc)
cor(college$Enroll_rate,college$Top25perc)
cor(college$Enroll_rate,college$Outstate)
cor(college$Enroll_rate,college$F.Undergrad)
cor(college$Enroll_rate,college$P.Undergrad)
cor(college$Enroll_rate,college$Room.Board)
cor(college$Enroll_rate,college$Books)
cor(college$Enroll_rate,college$Personal)
cor(college$Enroll_rate,college$PhD)
cor(college$Enroll_rate,college$Terminal)
cor(college$Enroll_rate,college$S.F.Ratio)
cor(college$Enroll_rate,college$perc.alumni)
cor(college$Enroll_rate,college$Expend)
cor(college$Enroll_rate,college$Grad.Rate)
plot(college$Enroll_rate,college$Outstate,main="Scatter plot of Enroll_rate versus Outstate",xlab="Enroll rate (Enroll divided by Apps)",ylab="Outstate")
plot(college$Enroll_rate,college$Room.Board,main="Scatter plot of Enroll_rate versus Room.Board",xlab="Enroll rate (Enroll divided by Apps)",ylab="Room.Board")
# 10.(a)
library(MASS)
Boston
?Boston
# how many rows?
nrow(Boston)
# how many cols?
ncol(Boston)
# 10.(b)
pairs(Boston[,1:14])
plot(Boston[,6],Boston[,7],main="Scatter plot of rm versus age",xlab=colnames(Boston)[6],ylab=colnames(Boston)[7])
plot(Boston[,8],Boston[,10],main="Scatter plot of dis versus tax",xlab=colnames(Boston)[8],ylab=colnames(Boston)[10])
# 10.(c)
cor(Boston)
## correlation with crim
Boston.corr[-1,1]
par(mfrow=c(2,2))
plot(Boston[,'rad'],Boston[,'crim'],main="Scatter plot of crim versus rad",xlab='rad',ylab='crim')
plot(Boston[,'tax'],Boston[,'crim'],main="Scatter plot of crim versus tax",xlab='tax',ylab='crim')
plot(Boston[,'lstat'],Boston[,'crim'],main="Scatter plot of crim versus lstat",xlab='lstat',ylab='crim')
plot(Boston[,'nox'],Boston[,'crim'],main="Scatter plot of crim versus nox",xlab='nox',ylab='crim')
# 10.(d)
par(mfrow=c(1,1))
count <- function(col_name,num) {
  counter=0
  for(i in 1:506){
    if(Boston[i,col_name] > num)
      counter = counter+1
  }
  print(num)
  return(counter)
}
## high crim
high_crim = mean(Boston[,'crim'])+2*sd(Boston[,'crim'])
count('crim',high_crim)
hist(Boston[,'crim'],main="Histogram of crim",xlab='crim',breaks=50)
summary(Boston[,'crim'])
## high tax
high_tax = mean(Boston[,'tax'])+1*sd(Boston[,'tax'])
count('tax',high_tax)
hist(Boston[,'tax'],main="Histogram of tax",xlab='tax',breaks=30)
summary(Boston[,'tax'])
## high ptratio
high_ptratio = mean(Boston[,'ptratio'])+1*sd(Boston[,'ptratio'])
count('ptratio',high_ptratio)
hist(Boston[,'ptratio'],main="Histogram of ptratio",xlab='ptratio',breaks=30)
summary(Boston[,'ptratio'])
# 10.(e)
table(Boston[,'chas'])
# 10.(f)
median(Boston[,'ptratio'])
# 10.(g)
for(i in 1:506) {
  if(Boston[i,'medv'] == min(Boston[,'medv']))
    min_data<-min_data+Boston[i,1:14]
}
## plot
plot(Boston[,'medv'],Boston[,'crim'],main="Scatter plot of medv versus crim",xlab='medv',ylab='crim')
points(x=Boston[399,1:14]$medv,y=Boston[399,1:14]$crim, pch=9,col="blue") 
points(x=Boston[406,1:14]$medv,y=Boston[406,1:14]$crim, pch=9,col="blue") 
plot(Boston[,'medv'],Boston[,'lstat'],main="Scatter plot of medv versus lstat",xlab='medv',ylab='lstat')
points(x=Boston[399,1:14]$medv,y=Boston[399,1:14]$lstat, pch=9,col="blue") 
points(x=Boston[406,1:14]$medv,y=Boston[406,1:14]$lstat, pch=9,col="blue") 
## print other predictors
Boston[399,1:14]
Boston[406,1:14]
# 10.(h)
summary(Boston[,'rm'])
hist(Boston[,'rm'],main="Distribution of Rooms by Dwelling", xlab="Rooms")
## rm>7
count_7 = 0
for(i in 1:506) {
  if(Boston[i,'rm'] > 7 )
    count_7 = count_7+1
}
print(count_7)
## rm>8
count_8 = 0
for(i in 1:506) {
  if(Boston[i,'rm'] > 8 )
    count_8 = count_8+1
}
print(count_8)
