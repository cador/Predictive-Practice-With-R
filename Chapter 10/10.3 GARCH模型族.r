library(quantmod)
setSymbolLookup(WK=list(name='000002.sz',src='yahoo'))
getSymbols("WK")
colnames(WK)
## [1] "000002.SZ.Open" "000002.SZ.High" "000002.SZ.Low" "000002.SZ.Close" 
## [5] "000002.SZ.Volume" "000002.SZ.Adjusted"
vdata=WK[400:2300,4]
plot(vdata,type='l')
modelData=vdata[1:1801]

library(forecast)
pacf(auto.arima(modelData)$residuals)

pacf(modelData)

#根据初始化参数求原时间序列的RSS，并返回
#modelData：原时间序列
#d：原时间序列的自回归阶数
#p：残差项滞后阶数
#q：条件方差滞后项
#r，w0，alphaI，faiI，rhoJ：条件方差相关参数
#phi0，phiI：原时间序列的自回归截距及系数
getRSSbyArgs<-function(modelData,d,p,q,r,w0,phi0,phiI,alphaI,faiI,rhoJ)
{
    n<-length(modelData)
    epsilonList=rep(0.1,p)
    htList=rep(0.1,q)
    baseData=NULL
    for(i in (d+1):n)
    {
        baseData=rbind(baseData,modelData[(i-1):(i-d)])
        xt=modelData[i]
        xt.fit=drop(c(phi0,phiI)%*%t(t(c(1,modelData[(i-1):(i-d)]))))
        epsilont=xt-xt.fit
        ht=(exp(w0)+sum(exp(alphaI)*(abs(epsilonList[1:p])-sin(faiI)*
           0.9999*epsilonList[1:p])^(exp(r)))+
                sum(exp(rhoJ)*htList[1:q]^(exp(r)/2)))^(2*exp(-r))
        #更新值epsilont和ht
        epsilonList=c(epsilont,epsilonList)
        htList=c(ht,htList)
    }
    return(sum((epsilonList)^2))
}

#参数平稳性验证
pwCheck<-function(r,alphaI,faiI,rhoJ)
{
    t0=(1/sqrt(2*pi))*exp(alphaI)*((1+sin(faiI)*0.9999)^r+(1-sin(faiI)*
       0.9999)^r)*2^((r-1)/2)*gamma((r+1)/2)
    t0=sum(t0)+sum(exp(rhoJ))
    return(t0<1)
}

#定义适应度函数
getAdjust <- function(x)
{
    r=x[1]
    phi0=x[2]
    phiI=x[3:(d+2)]
    w0=x[d+3]
    alphaI=x[(d+4):(d+3+p)]
    faiI=x[(d+4+p):(d+3+2*p)]
    rhoJ=x[(d+4+2*p):(d+3+2*p+q)]
    minCur=getRSSbyArgs(modelData,d,p,q,r,w0,phi0,phiI,alphaI,faiI,rhoJ)
    if(minCur<minRSS && pwCheck(r,alphaI,faiI,rhoJ))return(minCur)
    return(minRSS)
}

#初始化参数
n<-length(modelData)
d=1
p=10
q=5
library(genalg)
minRSS=1e+10
#定义监控函数
monitor<-function(rbga0)
{
    #打印种群个体的适应值
    print(rbga0$evaluations)
}
rbgaObj<-rbga(stringMin = c(0,-1,rep(-1,d),-1,rep(-5,p),rep(-1,p),rep(-5,q)), 
     stringMax = c(3,1,rep(1,d),1,rep(0,p),rep(1,p),rep(0,q)), popSize = 200, 
     iters = 20, mutationChance = 0.01, monitorFunc = monitor, 
     evalFunc = getAdjust, verbose = TRUE)
## Testing the sanity of parameters...
## Not showing GA settings...
## Starting with random values in the given domains...
## Starting iteration 1 
## Calucating evaluation values... ...............................done
## Sending current state to rgba.monitor()...
##   [1] 1.000000e+10 1.000000e+10 1.000000e+10 ......
## ......
## Creating next generation...
##   sorting results...
##   applying elitism...
##   applying crossover...
##   applying mutations... 45 mutations applied
## Starting iteration 20 
## Calucating evaluation values... ...............................done.
## Sending current state to rgba.monitor()...
##   [1] 2.379815e+02 5.169768e+03 5.818574e+03 5.818574e+03 ......

x=rbgaObj$population[which.min(rbgaObj$evaluations),]
x
## [1]  0.79360922  0.64640565  0.90727570  0.07158979 ......
r=x[1]
r
## [1] 0.7936092
phi0=x[2]
phi0
## [1] 0.6464056
phiI=x[3:(d+2)]
phiI
## [1] 0.9072757
w0=x[d+3]
w0
## [1] 0.07158979
alphaI=x[(d+4):(d+3+p)]
alphaI
## [1] -3.775042 -3.354314 -4.472956 -1.355353 -2.829966 -4.360019 ......
faiI=x[(d+4+p):(d+3+2*p)]
faiI
## [1]  0.6275878  0.1059777  0.7562138  0.5405060  0.8749887  0.6215253......
rhoJ=x[(d+4+2*p):(d+3+2*p+q)]
rhoJ
## [1] -4.698141 -2.576343 -3.623199 -4.963678 -2.134687

#进入迭代
errorList=NULL
for(iter in 1:1000)
{
    epsilonList=rep(0.1,p)
    htList=rep(0.1,q)
    baseData=NULL
    ht.dr.inner=NULL
    alphaI.d.inner=NULL
    faiI.d.inner=NULL
    rhoJ.d.inner=NULL
    for(i in (d+1):n)
    {
        baseData=rbind(baseData,modelData[(i-1):(i-d)])
        xt=modelData[i]
        xt.fit=drop(c(phi0,phiI)%*%t(t(c(1,modelData[(i-1):(i-d)]))))
        epsilont=xt-xt.fit
        com0=abs(epsilonList[1:p])-sin(faiI)*0.9999*epsilonList[1:p]
        com1=exp(alphaI)*com0^exp(r)
        ht=(exp(w0)+sum(com1)+
                sum(exp(rhoJ)*htList[1:q]^(exp(r)/2)))^(2*exp(-r))
        ht.dr.inner=c(sum(exp(2*r)*com1*log(com0))+
                          sum(0.25*exp(2*r+rhoJ)*htList[1:q]^(exp(r)/2)*
                          log(htList[1:q])),ht.dr.inner)
        alphaI.d.inner=rbind(com1,alphaI.d.inner)
        faiI.d.inner=rbind(exp(r+alphaI)*com0^(exp(r)-1)*
                     (-0.999*epsilonList[1:p])*cos(faiI),faiI.d.inner)
        rhoJ.d.inner=rbind(exp(rhoJ)*htList[1:q]^(exp(r)/2),rhoJ.d.inner)
        #更新值epsilont和ht
        epsilonList=c(epsilont,epsilonList)
        htList=c(ht,htList)
    }
    #更新参数向量
    #(1)求解一阶偏导
    epsilon.list=epsilonList[1:(length(epsilonList)-p)]
    ht.list=htList[1:(length(htList)-q)]
    phi0.d=-sum(epsilon.list/ht.list)
    phiI.d=(-(epsilon.list/ht.list)%*%baseData)[1,]
    ht.d=0.5*((ht.list-epsilon.list^2)/(ht.list^2))
    com2=2*exp(-r)*ht.list^(-0.5*exp(r))
    r.d=sum(ht.d*ht.list*(-log(ht.list)+com2*ht.dr.inner))
    w0.d=sum(ht.d*com2*exp(w0))
    alphaI.d=((ht.d*com2)%*%alphaI.d.inner)[1,]
    faiI.d=((ht.d*com2)%*%faiI.d.inner)[1,]
    rhoJ.d=((ht.d*com2)%*%rhoJ.d.inner)[1,]
    #(2)对参数进行更新
    phi0=phi0-phi0.d*0.001/n
    phiI=phiI-phiI.d*0.0001/n
    r=r-r.d*0.001/n
    w0=w0-w0.d*0.001/n
    alphaI=alphaI-alphaI.d/n
    faiI=faiI-faiI.d/n
    rhoJ=rhoJ-rhoJ.d/n
    print(sum(epsilonList^2))
    errorList=c(errorList,sum(epsilonList^2))
    if(length(errorList)>2 && 
       abs(rev(errorList)[2]-rev(errorList)[1])<1e-2)break
}
## [1] 237.9815
## [1] 236.9153
## [1] 235.8458
## ......
## [1] 133.7709
## [1] 133.7611
iter
## [1] 399

epsilonList=rep(0.1,p)
htList=rep(0.1,q)
vt=NULL
for(i in (d+1):n)
{
    xt=modelData[i]
    xt.fit=drop(c(phi0,phiI)%*%t(t(c(1,modelData[(i-1):(i-d)]))))
    epsilont=xt-xt.fit
    ht=(exp(w0)+sum(exp(alphaI)*(abs(epsilonList[1:p])-sin(faiI)*
       0.9999*epsilonList[1:p])^(exp(r)))+
            sum(exp(rhoJ)*htList[1:q]^(exp(r)/2)))^(2*exp(-r))
    vt=c(vt,epsilont/sqrt(ht))
    #更新值epsilont和ht
    epsilonList=c(epsilont,epsilonList)
    htList=c(ht,htList)
}
pacf(vt)

#预测后继100期，并与真实结果比较
library(forecast)
predVals=NULL
trueVals=NULL
predhts=NULL
for(i in 1:100)
{
    vt.arima=auto.arima(vt)
    vt.fit=predict(vt.arima,n.ahead=1)$pred
    ht=(exp(w0)+sum(exp(alphaI)*(abs(epsilonList[1:p])-sin(faiI)*
       0.9999*epsilonList[1:p])^(exp(r)))+
       sum(exp(rhoJ)*htList[1:q]^(exp(r)/2)))^(2*exp(-r))
    x.fit=sum(c(1,vdata[(n+i-1):(n-d+i)])*c(phi0,phiI))
    epsilont=vdata[n+i]-x.fit
    x.fit=x.fit+sqrt(ht)*vt.fit
    htList=c(ht,htList)
    epsilonList=c(epsilont,epsilonList)
    predVals=c(predVals,x.fit)
    trueVals=c(trueVals,vdata[n+i])
    vt=c(vt,epsilont/sqrt(ht))
    predhts=c(predhts,ht)
}

k=200
plot(c(vdata[(n-k):n],trueVals),type='l',ylim=c(8,16))
lines((k+2):(100+k+1),predVals,col='red')

plot(predhts,pch=20)
lines(predhts)

vdata=read.csv("f:\\vdata001.csv",header=T)
vdata=vdata[,2]
plot(vdata,type='l')
modelData=vdata[1:1801]
library(fGarch)
n<-length(modelData)
#初始化预测值、真实值、以及波动标准差向量
predVals=NULL
trueVals=NULL
predhts=NULL
for(i in 1:100)
{
    a=garchFit(~garch(1,1), data = vdata[1:(n+i-1)],trace=F,include.mean=F)
    pred=predict(a,n.ahead=1)
    predVals=c(predVals,pred$meanError)
    predhts=c(predhts,pred$standardDeviation)
    trueVals=c(trueVals,vdata[n+i])
}
k=200
plot(c(vdata[(n-k):n],trueVals),type='l',ylim=c(8,16))
lines((k+2):(100+k+1),predVals,col='red')

