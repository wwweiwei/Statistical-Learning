bank= read.table("bank-additional-full.csv",header=TRUE,sep=";")

#install.packages("funModeling")
library(funModeling)
for (i in 1:nrow(bank)) {
  for (j in 1:ncol(bank)) {
    if(bank[i,j]=="unknown") bank[i,j] = NA 
  }
}
#將unknown改為NA

#數值缺失或異常
status(bank)

#敘述統計量
summary(bank)
#install.packages("psych")
library(psych)
describe(bank)

#資料分配
plot_num(bank)

freq(data=bank, input = c('job','marital','education','default','housing','loan','contact','month','day_of_week','poutcome','y'))

#變數間的交互作用關係
#連續型
bank[,21]=as.numeric(bank$y)
nums <- unlist(lapply(bank, is.numeric)) 
bank.n=bank[,nums]
corr <- round(cor(bank.n), 2)
corr
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr, lab=TRUE)

#類別型
bank = read.table("bank-additional-full.csv",header=TRUE,sep=";")
#install.packages("dlookr")
library(dlookr)
y.bank <- target_by(bank, y)
num2 <- unlist(lapply(bank, is.factor)) 
bank.f=bank[,num2]
par(mfrow=c(2,2))
for (i in 1:20) {
  if(num2[i]=="TRUE")
  plot(relate(y.bank, colnames(bank[i])))
}