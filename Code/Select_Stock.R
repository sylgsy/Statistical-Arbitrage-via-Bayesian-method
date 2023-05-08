#load packages
library(lmtest)
library(tseries)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(factoextra)

a <- list.files("Stocks")
dir <- paste("./Stocks/",a,sep="")   #path dir
num_stock <- 100  #number of stock
reg_num <- 500   #number of train
test_num <- 100
index_pre <- seq(100,7000,by = 10)
index <- vector()
#Remove files with small sample size
for (i in 1:length(index_pre)){
  x <- try(read.csv(file = dir[index_pre[i]],header=T,sep=","))
  while (length(index) < num_stock && !inherits(x, 'try-error')){
    if (nrow(x) > reg_num + test_num){
      index <- append(index,index_pre[i])
    }
    break
  }
  
  if (length(index) == num_stock){
    break
  }
}


profit <- matrix(0,num_stock,reg_num)
for (i in 1:num_stock){
  stock <- read.csv(file = dir[index[i]],header=T,sep=",")
  for (j in 1:reg_num){
    profit[i,j] <- log(stock[nrow(stock) - reg_num + j,5]/stock[nrow(stock) - reg_num +j-1,5])
  }
}
average_profit <- apply(profit,1,mean)
vol_profit <- apply(profit,1,sd)
mean_var <- cbind(average_profit,vol_profit)
mean_var[,2] <- mean_var[,2]

name <- vector()
for (i in 1:num_stock){
  name <- append(name,dir[index[i]])
}
name <- gsub("./Stocks/","",name)
name <- gsub(".us.txt","",name)
rownames(mean_var) <- name
head(mean_var)  #return and volatility


fit <- kmeans(mean_var, 4)
fviz_cluster(fit, data = mean_var,main = "",stand = FALSE,labelsize = 9,pointsize = 0.5,repel = TRUE,geom = c("point","text"))

clus1 <- which(fit[["cluster"]] == 1)
clus2 <- which(fit[["cluster"]] == 2)
clus3 <- which(fit[["cluster"]] == 3)
clus4 <- which(fit[["cluster"]] == 4)

clus1
clus2
clus3
clus4

#index 4 21
stock4 <- read.csv(file = dir[index[4]],header=T,sep=",")
stock21 <- read.csv(file = dir[index[21]],header=T,sep=",")
stock4 <- stock4[,-c(2,3,4,6,7)]
stock21 <- stock21[,-c(2,3,4,6,7)]
stock <- merge(stock4,stock21,by = "Date")
ggplot()+
  geom_line(data = stock, aes(x=as.Date(stock[,1]), y = stock[,2]),color="black")+
  geom_line(data = stock, aes(x=as.Date(stock[,1]), y = stock[,3]),color = "red")+
  xlab("Time")+
  ylab("Stock Price")

#index 44 50
stock44 <- read.csv(file = dir[index[44]],header=T,sep=",")
stock50 <- read.csv(file = dir[index[50]],header=T,sep=",")
stock44 <- stock44[,-c(2,3,4,6,7)]
stock50 <- stock50[,-c(2,3,4,6,7)]
stock <- merge(stock44,stock50,by = "Date")
ggplot(data = stock, aes(x=as.Date(stock[,1])))+
  geom_line(aes(y = stock[,2],color="bbk.us"))+
  geom_line(aes(y = stock[,3],color = "bfy.us"))+
  scale_color_manual(name = "", 
                     values = c("bbk.us" = "red", "bfy.us" = "blue")) +
  xlab("Time")+
  ylab("Price")


adf_matrix <- matrix(1,2,length(clus1))
for (i in 1:length(clus1)){
  x <- read.csv(file = dir[index[clus1[i]]],header=T,sep=",")
  x <- x[,-c(2,3,4,6,7)]
  reg_end <- nrow(x)
  reg_start <- max(1,reg_end - reg_num)
  sta_x <- suppressWarnings(adf.test(x[reg_start:reg_end,2]))
  adf_matrix[1,i] = sta_x[["p.value"]]
  sta_diff_x <- suppressWarnings(adf.test(diff(x[reg_start:reg_end,2])))
  adf_matrix[2,i] = sta_diff_x[["p.value"]]
}

adf_matrix[,1:10]

s <- read.csv(file = dir[index[1]],header=T,sep=",")
ggplot()+
  geom_line(data = s, aes(x=as.Date(s[,1]), y = s[,5]),color="black")+
  geom_line(data = s, aes(x=as.Date(s[,1]), y = c(0,diff(s[,5]))),color = "red")+
  xlab("Time")+
  ylab("")

pvalue_matrix <- matrix(1,length(clus1),length(clus1))
for (i in 1:length(clus1)){
  for (j in i:length(clus1)){
    if (i != j){
      x <- read.csv(file = dir[index[clus1[i]]],header=T,sep=",")
      x <- x[,-c(2,3,4,6,7)]
      y <- read.csv(file = dir[index[clus1[j]]],header=T,sep=",")
      y <- y[,-c(2,3,4,6,7)]
      data = merge(x,y,by = "Date")
      reg_end <- nrow(data)
      reg_start <- max(1,reg_end - reg_num)
      reg <- lm(data[reg_start:reg_end,2]~data[reg_start:reg_end,3])
      error <- residuals(reg)
      ts <- suppressWarnings(adf.test(error))
      pvalue <- ts[["p.value"]]
      pvalue_matrix[i, j] <- pvalue
    }
  }
}


#heat map
a <- melt(pvalue_matrix)
a[,3] <- round(a[,3],digits = 2)
ggplot(data = a, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+
  scale_fill_gradient2(low='green', high='#FF7256', mid='white')+ xlab("Stock index") + ylab("stock index") + geom_text(aes(Var1, Var2, label = value), color = "black", size = 3)

