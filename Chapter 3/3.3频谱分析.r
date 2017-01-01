specOut<-spec.pgram(as.integer(AirPassengers),taper=0,log='no')
maxSpecFreq<-specOut$freq[which.max(specOut$spec)]
abline(v=maxSpecFreq,lty=2,col='red')
period<-1/maxSpecFreq
period
