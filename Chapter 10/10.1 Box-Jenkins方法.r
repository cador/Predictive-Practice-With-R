#加载基础数据
tsData<-read.csv("f:\\basexl.csv",header=T)
#转换成时序数据
tsData<-ts(tsData$农业,start=tsData$年份[1])
n<-length(tsData)
plot.ts(tsData)
#此处预留10年的数据进行验证
orgPred=tsData[(n-9):n]
tsData=tsData[1:(n-10)]

#进行d阶差分运算
d=1
z=NULL
for(t in (d+1):length(tsData))
{
    tmp=0
    for(i in 0:d)
    {
     tmp=tmp+(-1)^i*(factorial(d)/(factorial(i)*factorial(d-i)))*(tsData[t-i])
    }
    z=c(z,tmp)
}
#使用单位根检验差分后序列的平稳性
library(fUnitRoots)
unitrootTest(z)
## 
## Title:
##  Augmented Dickey-Fuller Test
## 
## Test Results:
##   PARAMETER:
##     Lag Order: 1
##   STATISTIC:
##     DF: -3.0557
##   P VALUE:
##     t: 0.003728 
##     n: 0.2121 
#使用Ljung-Box检验差分后的序列是否是白噪声
Box.test(z, type="Ljung-Box")
## 
##  Box-Ljung test
## 
## data:  z
## X-squared = 7.7252, df = 1, p-value = 0.005445

#分别绘制差分后序列的自相关图和偏自相关图
par(mfrow=c(1,2))
acf(z)
pacf(z)
par(mfrow=c(1,1))

#基于最小化残差平方和的假设，使用梯度下降法拟合未知参数
miu=mean(z)
miu
## [1] 2.353846
theta1=0.5
lambda=0.0001
epsilon_theta1=0
errorList=NULL
for(iter in 1:60)
{
    epsilon=0
    error=0
    for(i in 1:length(z))
    {
        epsilon_theta1=epsilon+theta1*epsilon_theta1
        theta1=theta1-lambda*2*(z[i]-miu+theta1*epsilon)*epsilon_theta1
        epsilon=z[i]-miu+theta1*epsilon
        error=error+epsilon^2
    } 
    errorList=c(errorList,error)
    print(paste("iter:",iter," error:",error))
    #当连续两次残差平方和的差小于1e-5时，退出循环
    if(length(errorList)>2 && 
       abs(rev(errorList)[2]-rev(errorList)[1])<1e-5)break
}
## [1] "iter: 1  error: 2158.55284121236"
## [1] "iter: 2  error: 1629.11757143953"
## [1] "iter: 3  error: 1378.697932326"
## [1] "iter: 4  error: 1235.21904747208"
## [1] ......
## [1] "iter: 40  error: 863.623233001729"
#最终求得参数theta1为
theta1
## [1] -0.7940838

error=NULL
epsilon=0
for(i in 1:length(z))
{
    epsilon=z[i]-miu+theta1*epsilon
    error=c(error,epsilon)
}
#使用Ljung-Box检验error序列是否为白噪声
Box.test(error, type="Ljung-Box")
## 
##  Box-Ljung test
## 
## data:  error
## X-squared = 0.079324, df = 1, p-value = 0.7782

#基于该模型对差分后的序列进行预测
predX=miu+mean(error)-theta1*epsilon
predX
#由于经过1阶差分的运算，所以此处需要进行差分的逆运算，以计算原始序列对应的预测值
tsLen<-length(tsData)
org.predX=tsData[tsLen]+predX
org.predX
#对超过1期的预测值，统一为predXt
predXt=org.predX+2.353846+1.7940838*mean(error)
#绘制出原始值和预测值
plot(c(tsData,orgPred),type='l',col='darkgreen',lwd=2)
lines(tsLen:(tsLen+10),c(tsData[tsLen],c(org.predX,rep(predXt,9))),col='red')
points(tsLen+1,org.predX,pch=20,col='red')

predX=NULL
for(i in 1:10)
{
    predval=miu+mean(error)-theta1*epsilon
    if(i==1){
        org.predX=tsData[tsLen]+predval
    }else{
        org.predX=orgPred[i-1]+predval
    }
    predX=c(predX,org.predX)
    epsilon=orgPred[i]-org.predX
    error=c(error,epsilon)
}
plot(c(tsData,orgPred),type='l',col='darkgreen',lwd=2)
lines(tsLen:(tsLen+10),c(tsData[tsLen],predX),col='red')
points((tsLen+1):(tsLen+10),predX,col='red',pch=20)

#加载基础数据
tsData<-read.csv("f:\\basexl.csv",header=T)
#转换成时序数据
tsData<-ts(tsData$农业,start=tsData$年份[1])
n<-length(tsData)
plot.ts(tsData)
#此处预留10年的数据进行验证
orgPred=tsData[(n-9):n]
tsData=tsData[1:(n-10)]
#设置预测值向量初始值
preds<-NULL
newdata<-NULL
for(i in 1:10)
{
    ts0=c(tsData,newdata)
    arima.obj<-Arima(ts0,order=c(0,1,1))
    a=forecast(arima.obj,h=1)
    preds=c(preds,a$mean[1])
    newdata=orgPred[1:i]
}
plot(c(tsData,orgPred),type='l',col='darkgreen',lwd=2)
lines(tsLen:(tsLen+10),c(tsData[tsLen],preds),col='red')
points((tsLen+1):(tsLen+10),preds,col='red',pch=20)


preds<-NULL
newdata<-NULL
for(i in 1:10)
{
    ts0=c(tsData,newdata)
    arima.obj<-auto.arima(ts0)
    a=forecast(arima.obj,h=1)
    preds=c(preds,a$mean[1])
    newdata=orgPred[1:i]
}
plot(c(tsData,orgPred),type='l',col='darkgreen',lwd=2)
lines(tsLen:(tsLen+10),c(tsData[tsLen],preds),col='red')
points((tsLen+1):(tsLen+10),preds,col='red',pch=20)


