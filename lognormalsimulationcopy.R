require(quantmod)

stock <- na.omit( 
  getSymbols(
    "CABK.MC",
    src = "yahoo",
    from = "2019-01-01",
    auto.assign = FALSE
  )
)



# choose use monthly/daily data
#stock.monthly <- stock[endpoints(stock, on ="months")]
stock.daily <- stock[endpoints(stock, on ="days")]

# base plot of time series prior to xts
# get the data in data.frame format rather than xts
stock.df <- data.frame(
  index(stock.daily),
  coredata(stock.daily),
  stringsAsFactors=FALSE
)
head(stock.df)
# name columns
colnames( stock.df ) <- c( "Date", "Open", "High", "Low", "Close", "Vol","Adj" )
# go back in time to plot.default from the graphics library
graphics::plot.default(
  x = stock.df$Date,
  y = stock.df$Adj,
  type = "l",
  xlab = "Data",
  ylab = "Tancament",
  main = "CaixaBank"
)

ret<-log(stock.df[2:nrow(stock.df),7])-log(stock.df[1:(nrow(stock.df)-1),7])
mu<-mean(ret)*255
vol<-sd(ret)*sqrt(255)
hist(ret, breaks=50,main=c(paste("Annual Mean: ",round(mu*100,2),"Annual sd: ",round(vol*100,2))),xlab="Daily Return")
graphics::plot.default(  x = stock.df$Date[2:nrow(stock.df)],y=ret, 
  type = "l",
  xlab = "Data",
  ylab = "Daily Return",
  main = "CaixaBank"
)

# Simulation of 255 days
set.seed(123)
Z <- rnorm(255,0,1)   # normal 0,1
u <- mean(ret)        # Expected daily return
sd <- sd(ret)         # Expected daily sd
s <- stock.df[nrow(stock.df),7]              # initial price
fcast <- c(s)         # initiate  vector
a <- 2                
t <- 1:256            

for(i in Z){
  S <- s*exp(u-sd^2/2+sd*i)
  fcast[a] <- S                        
  s <- S                                 
  a <- a + 1
}

plot(t,fcast,main="Forecasted Prices",xlab="time",ylab="price", type="l",col="blue")
fcast
ret2<-log(fcast[2:length(fcast)])-log(fcast[1:(length(fcast)-1)])
fcastmu=mean(ret2)*255
fcastvol=sd(ret2)*sqrt(255)
hist(ret2, breaks=50,main=c(paste("Annual Mean: ",round(fcastmu*100,2),"Annual sd: ",round(fcastvol*100,2))),xlab="Daily Return")


eixx <-stock.df$Date
for (i in t) {
eixx[length(stock.df$Date)+i] <- stock.df$Date[length(stock.df$Date)]+i 
}
eixy<- c((stock.df$Adj), (fcast))


graphics::plot.default(eixx,
  eixy,
  type = "l",
  xlab = "Data",
  ylab = "Adj Price",
  main = c(paste("CaixaBank", nrow(stock.df)-length(fcast),"days forecast since ",stock.df$Date[nrow(stock.df)])))



