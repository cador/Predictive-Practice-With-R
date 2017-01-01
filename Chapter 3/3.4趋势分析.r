library(TTR)
plot(AirPassengers)
ap.sma=SMA(AirPassengers,n=10)
lines(ts(ap.sma,start=1949,end=1961,frequency = 12),col='red',lwd=2)

ap.ema=EMA(AirPassengers,n=10,ratio=0.15)
lines(ts(ap.ema,start=1949,end=1961,frequency = 12),col='blue',lwd=2,lty=2)

