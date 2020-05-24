
Retorns<- c(ret, 0)
Date<-as.Date(file[,c("Date")])
Datamés257<-Date[c(257)]+1:257
Adj.Close<-file[,c("Adj.Close")]
Last.Adj.Close<-Adj.Close[c(257)]
e<-exp(1)
Adj.Close.Future<-Last.Adj.Close*e^(mean(ret)*1:257)
serie=cbind(serie,Retorns, Datamés257, Adj.Close.Future)
serie