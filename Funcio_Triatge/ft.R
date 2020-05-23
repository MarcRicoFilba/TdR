file <- read.csv("CABK.MC.csv")
serie <- file[,c("Date","Adj.Close")]
ret<-log(serie[2:nrow(serie),2])-log(serie[1:(nrow(serie)-1),2])
ret
mean(ret)
sd(ret)
