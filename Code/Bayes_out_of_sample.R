library(lmtest)
library(tseries)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(factoextra)
library(MCMCpack)

a <- list.files("Stocks")
dir <- paste("./Stocks/",a,sep="")   #path dir

index_stock1 <- 660 #bbk
index_stock2 <- 740 #bfy
stock1 <- read.csv(file = dir[index_stock1],header=T,sep=",")
stock1 <- stock1[,-c(2,3,4,6,7)]   #660
stock2 <- read.csv(file = dir[index_stock2],header=T,sep=",")
stock2 <- stock2[,-c(2,3,4,6,7)]   #740
data <- merge(stock1,stock2,by = "Date")
#data <- data[967:nrow(data),]

test_num <- 1000
reg_num <- 2197

reg_end <- max(nrow(data) - test_num,floor(nrow(data)/2))
reg_start <- max(1,reg_end - reg_num + 1)

reg <- lm(data[reg_start:reg_end,2]~data[reg_start:reg_end,3])
error <- residuals(reg)
ts <- adf.test(error)
pvalue <- ts[["p.value"]]

coff <- as.numeric(reg[["coefficients"]][2])
average <- mean(data[reg_start:reg_end,2]-coff*data[reg_start:reg_end,3])
std <- sd(data[reg_start:reg_end,2]-coff*data[reg_start:reg_end,3])
ggplot(data = data[reg_start:nrow(data),], aes(x=as.Date(data[reg_start:nrow(data),1])))+
  geom_line(aes(y = data[reg_start:nrow(data),2],color="bbk.us"))+
  geom_line(aes(y = data[reg_start:nrow(data),3],color = "bfy.us"))+
  scale_color_manual(name = "", 
                     values = c("bbk.us" = "red", "bfy.us" = "blue")) +
  xlab("Time")+
  ylab("value of portfolio")


mu0 <- average   # 均值的先验分布的均值
k0 <- 0.01 # 均值的先验分布的精度（kappa）
s20 <- 1   # 方差的先验分布的初始值
v0 <- 2*std*std/(std*std - 1)   # 方差的先验分布的初始值（自由度）
set.seed(123)
posterior <- MCMCregress(data[reg_start:nrow(data),2]-coff*data[reg_start:nrow(data),3] ~ 1, 
                           prior = list(mu = c(mu0, k0), sigma = c(s20, v0)),
                           mcmc = 10000, burnin = 1000)
Pos <- summary(posterior)
aver_std <- Pos[["statistics"]]

average <- aver_std[1,1]
std <- sqrt(aver_std[2,1])
var <- rep(std*std,test_num)
mu_average <- rep(1,test_num)
var_average <- rep(1,test_num)
mu_average[1] <- average
var_average[1] <- var[1]/reg_num
for (i in 2:test_num){
  mu_average[i] <- (1/var_average[i-1])*mu_average[i-1] + (1/var[i-1])*(data[reg_end + i,2] - coff*data[reg_end + i,3])
  mu_average[i] <- mu_average[i]/(1/var_average[i-1] + 1/var[i-1])
  var_average[i] <- 1/(1/var_average[i-1] + 1/var[i-1])
  var[i] <- var_average[i]*(reg_num+i-1)
}

r <- 0.03

money <- vector()
money <- append(money,10000)
volume <- vector()
volume <- append(volume ,0)
flag <- vector()
flag <- append(flag,list(c(data[reg_end,1],"平仓")))

for(i in (reg_end + 1):nrow(data)){
  if (flag[[length(flag)]][2] == "做多"){
    money <- append(money,money[length(money)] + volume[length(volume)]*(data[i,2]-coff*data[i,3] - (data[i-1,2]-coff*data[i-1,3])))
  }
  
  if (flag[[length(flag)]][2] == "做空"){
    money <- append(money,money[length(money)] - volume[length(volume)]*(data[i,2]-coff*data[i,3] - (data[i-1,2]-coff*data[i-1,3])))
  }
  
  if (flag[[length(flag)]][2] == "平仓"){
    money <- append(money,money[length(money)] + volume[length(volume)]*(data[i,2]-coff*data[i,3] - data[i-1,2]-coff*data[i-1,3]))
  }
  
  if (flag[[length(flag)]][2] == "平仓" && data[i,2]-coff*data[i,3] > mu_average[i-reg_end] + 0.5*std && data[i,2]-coff*data[i,3] < data[i-1,2]-coff*data[i-1,3]){
    flag <- append(flag,list(c(data[i,1],"做空")))
    volume <- append(volume, 10000/abs(data[i-1,2]-coff*data[i-1,3]))
  }
  
  if (flag[[length(flag)]][2] == "平仓" && data[i,2]-coff*data[i,3] < mu_average[i-reg_end] - 0.5*std && data[i,2]-coff*data[i,3] > data[i-1,2]-coff*data[i-1,3]){
    flag <- append(flag,list(c(data[i,1],"做多")))
    volume <- append(volume, 10000/abs(data[i-1,2]-coff*data[i-1,3]))
  }
  
  if (flag[[length(flag)]][2] == "做空" && data[i,2]-coff*data[i,3] - mu_average[i-reg_end]  < 0*sqrt(var[i-reg_end])){
    flag <- append(flag,list(c(data[i,1],"平仓")))
    volume <- append(volume,0)
  }
  
  if (flag[[length(flag)]][2] == "做多" && data[i,2]-coff*data[i,3] - mu_average[i-reg_end]  > 0*sqrt(var[i-reg_end])){
    flag <- append(flag,list(c(data[i,1],"平仓")))
    volume <- append(volume,0)
  }
  
  if (flag[[length(flag)]][2] == "做空" && data[i,2]-coff*data[i,3] - mu_average[i-reg_end] > 3*sqrt(var[i-reg_end]) && which(data[,1] == flag[[length(flag)]][1]) != i){
    flag <- append(flag,list(c(data[i,1],"平仓")))
    break
  }
  
  if (flag[[length(flag)]][2] == "做多" && data[i,2]-coff*data[i,3] - mu_average[i-reg_end] < -3*sqrt(var[i-reg_end]) && which(data[,1] == flag[[length(flag)]][1]) != i){
    flag <- append(flag,list(c(data[i,1],"平仓")))
    break
  }
}

plot(as.Date(data[(reg_end+1):nrow(data),1]), data[(reg_end+1):nrow(data),2]-coff*data[(reg_end+1):nrow(data),3],type = "l",xlab="Time",ylab="value of portfolio")
lines(as.Date(data[(reg_end+1):nrow(data),1]),mu_average)
lines(as.Date(data[(reg_end+1):nrow(data),1]),mu_average - 0.5*sqrt(var[i-reg_end]),col="red")
lines(as.Date(data[(reg_end+1):nrow(data),1]),mu_average + 0.5*sqrt(var[i-reg_end]),col="red")

for (i in 1:length(flag)){
  if (flag[[i]][2] == "平仓"){
    points(as.Date(flag[[i]][1]),data[which(data[,1] == flag[[i]][1]),2] - coff*data[which(data[,1] == flag[[i]][1]),3],col='blue',cex = 1.5,pch=18)
  }
  if (flag[[i]][2] == "做多"){
    points(as.Date(flag[[i]][1]),data[which(data[,1] == flag[[i]][1]),2] - coff*data[which(data[,1] == flag[[i]][1]),3],col='green',cex = 1.5,pch=18)
  }
  if (flag[[i]][2] == "做空"){
    points(as.Date(flag[[i]][1]),data[which(data[,1] == flag[[i]][1]),2] - coff*data[which(data[,1] == flag[[i]][1]),3],col='red',cex = 1.5,pch=18)
  }
}

ggplot()+
  geom_line(data = data[(reg_end+1):nrow(data),], aes(x=as.Date(data[(reg_end+1):nrow(data),1]), y = money[2:length(money)]))+
  xlab("Time")+
  ylab("Value")    

profit <- vector()
for (i in 1:floor((length(flag)-1)/2)){
  estimate_begin <- which(data[,1] == flag[[2*i]][1]) - reg_end + 1
  estimate_end <- which(data[,1] == flag[[2*i + 1]][1]) - reg_end
  for (j in 1:estimate_end - estimate_begin){
    profit <- append(profit,log(money[j + estimate_begin]/money[j + estimate_begin - 1]))
  }
}

average_profit <- mean(profit)
vol_profit <- sd(profit)
average_profit <- average_profit*250
vol_profit <- vol_profit*sqrt(250)
sharpe_ann <- (average_profit-r)/vol_profit

log_re <- rep(0,length(money)-1)
for(i in 1:length(log_re)){
  log_re[i]<-log(money[i+1]/money[i])
  }
rate_ann <- 250*mean(log_re)

cat("annual_rate:", rate_ann, "\n")
cat("sharpe_ratio:", sharpe_ann, "\n")
# 初始化最大回撤
max_drawdown <- 0

# 遍历每个交易周期
for (i in 1:length(money)) {
  # 计算当前交易周期内的最大回撤
  drawdown <- 1 - (money[i] / max(money[1:i]))
  # 更新最大回撤
  if (drawdown > max_drawdown) {
    max_drawdown <- drawdown
  }
}

# 输出最大回撤
cat("max_drawdown:", max_drawdown, "\n")                         

