SP500 <- read.csv("/Users/liangguo/Desktop/PhD/博二下-梁果/高等应用统计/StockPricePredictor-master/Data from YahooFinance/SP500.csv")
SP500 <- SP500[2601:5797,]

alpha <- rep(0,length(money))
for (i in 1:length(money)){
  alpha[i] <- (money[i]/10000) - (SP500[nrow(SP500)-test_num+i,5]/SP500[(nrow(SP500)-test_num),5])
  alpha[i] <- 1 + alpha[i]
}

ggplot(data = data[reg_end:nrow(data),], aes(x=as.Date(data[reg_end:nrow(data),1])))+
  geom_line(aes(y = money/10000,color = "bbk-bfy"))+
  geom_line(aes(y = SP500[(nrow(SP500)-test_num):nrow(SP500),5]/SP500[(nrow(SP500)-test_num),5], color = "S&P500"))+
  geom_line(aes(y = alpha, color = "Excess Return"))+
  scale_color_manual(name = "", 
                     values = c("bbk-bfy" = "yellow", "S&P500" = "red","Excess Return" = "grey")) +
  xlab("Time")+
  ylab("Profit") 

