#自定义函数计算数值预测模型的评估指标
#trueValue:真实值
#predValue:预测值
#return-->list(...)
ppe<-function(trueValue,predValue)
{
    #1.计算绝对误差（Absolute Error，简记为E）
    e.E<-trueValue-predValue
    #2.计算相对误差（Relative Error，简记为e）
    e.e<-e.E*sign(trueValue)/(abs(trueValue)+exp(-10))
    #3.计算平均绝对误差（Mean Absolute Error，简记为MAE）
    e.MAE<-mean(abs(e.E))
    #4.计算均方误差（Mean Squared Error，简记为MSE）
    e.MSE<-mean(e.E^2)
    #5.计算归一化均方误差（Normalized Mean Squared Error，简记为NMSE）
    e.NMSE<-sum(e.E^2)/sum(trueValue-mean(trueValue))
    #6.计算均方根误差（Root Mean Squared Error，简记为RMSE）
    e.RMSE<-sqrt(e.MSE)
    #7.计算平均绝对百分误差（Mean Absolute Percentage Error，简记为MAPE）
    e.MAPE<-mean(abs(e.e))
    #8.计算希尔不等系数（Theil inequality coefficient，简记为TIC）
    e.TIC<-e.RMSE/(sqrt(mean(trueValue^2))+sqrt(mean(predValue^2)))
    #9.计算判定系数（Coefficient Of Determination，一般记为R^2）
    e.R2<-1-sum(e.E^2)/sum((trueValue-mean(trueValue))^2)
    return(list(e.E=e.E,e.e=e.e,e.MAE=e.MAE,e.MSE=e.MSE,e.NMSE=e.NMSE,
                e.RMSE=e.RMSE,e.MAPE=e.MAPE,e.TIC=e.TIC,e.R2=e.R2))
}

lm.fit<-lm(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=iris)
y<-iris$Petal.Width
yhat<-fitted(lm.fit)
#获得真实值的标准差
sd0<-sd(y)
#n倍标准差
n<-1
#等分为100份
k<-100
nV<-(1:(n*k))/k
vals<-NULL
for(i in nV){
    vals<-c(vals,NROW(yhat[yhat>=(y-i*sd0) & yhat<=(y+i*sd0)])/NROW(y))
}
vals<-100*vals
plot(vals,col='white',xlab=paste(k,"分",n,"倍标准差",sep=""),ylab="%累计收益率",
    main="一倍标准差累计收益图")
lines(vals,col='blue')
abline(h=100,col='gray',lty=2)

