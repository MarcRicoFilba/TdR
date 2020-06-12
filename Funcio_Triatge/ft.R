library(quantmod)   # get stock prices; useful stock analysis functions
symbols <- c("CABK.MC") 
serie <- getSymbols(symbols, src = 'yahoo', 
             from = "2019-01-01",
             to = "2020-06-01",
             auto.assign = TRUE, warnings = FALSE) 
serie <-get(serie)
#file <- read.csv("CABK.MC.csv")
serie <- serie[,6]
serie<-as.data.frame(serie)
ret<-log(serie[2:nrow(serie),1])-log(serie[1:(nrow(serie)-1),1])
mean(ret)
sd(ret)
hist(ret, breaks=50)
