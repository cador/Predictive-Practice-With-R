library(quantmod)
setSymbolLookup(GZMT=list(name='600519.ss',src='yahoo'))
getSymbols("GZMT")
## [1] "GZMT"
head(GZMT)
##            600519.SS.Open    600519.SS.High    600519.SS.Low ... 600519.SS.Adjusted
## 2007-01-01       85.48999       92.10000      84.56003 ...         56.11134
## 2007-01-02       85.48999       92.10000      84.56003 ...         56.11134
## 2007-01-03       85.48999       92.10000      84.56003 ...         56.11134
## 2007-01-04       90.00994       90.18004      85.49997 ...         54.84002
## 2007-01-05       84.79002       86.69002      83.29997 ...         55.11468
## 2007-01-08       86.30004       86.60005      83.88002 ...         54.21392
chartSeries(GZMT)
library(quantmod)
setSymbolLookup(GZMT=list(name='600519.ss',src='yahoo'))
getSymbols("GZMT")
GZMT=data.frame(GZMT)
N=nrow(GZMT)
subdata=GZMT[1:(N-30),1:4]
#由于后面需要对误差进行累加，此处对所有指标，按均值和标准差进行标准化处理
subdata=scale(subdata)
center.back=attr(subdata,"scaled:center")
scale.back=attr(subdata,"scaled:scale")
#加载fUnitRoots，使用adfTest进行单位根检验
library(fUnitRoots)
for(i in 1:ncol(subdata))
{
    pValue=adfTest(subdata[,i])@test$p.value
    print(paste("指标",colnames(subdata)[i],"单位根检验的p值为：",pValue))
}
## [1] "指标 X600519.SS.Open 单位根检验的p值为： 0.01"
## [1] "指标 X600519.SS.High 单位根检验的p值为： 0.01"
## [1] "指标 X600519.SS.Low 单位根检验的p值为： 0.01"
## [1] "指标 X600519.SS.Close 单位根检验的p值为： 0.01"

#模型阶数从1开始逐一增加
rowCol=dim(subdata)
aicList=NULL
lmList=list()
for(p in 1:10)
{
    baseData=NULL
    for(i in (p+1):rowCol[1])
    {
        baseData=rbind(baseData,c(as.vector(subdata[i,]),
                                  as.vector(subdata[(i-1):(i-p),])))
    }
    X=cbind(1,baseData[,(rowCol[2]+1):ncol(baseData)])
    Y=baseData[,1:rowCol[2]]
    coefMatrix=solve(t(X)%*%X)%*%t(X)%*%Y
    aic=log(det(cov(Y-X%*%coefMatrix)))+
        2*(nrow(coefMatrix)-1)^2*p/nrow(baseData)
    aicList=c(aicList,aic)
    lmList=c(lmList,list(coefMatrix))
}
#对比查看阶数和AIC
data.frame(p=1:10,aicList)
##     p   aicList
## 1   1 -23.80727
## 2   2 -23.79840
## 3   3 -23.58177
## 4   4 -23.09444
## 5   5 -22.28852
## 6   6 -21.07052
## 7   7 -19.35058
## 8   8 -17.05341
## 9   9 -14.10079
## 10 10 -10.41445

p=which.min(aicList)
n=nrow(subdata)
preddf=NULL
subdata=data.frame(subdata)
for(i in 1:30)
{
    predData=as.vector(subdata[(n+i-1):(n+i-p),])
    predVals=c(1,t(predData)[,1])%*%lmList[[p]]
    #使用均值和标准差，还原预测值
    predVals=predVals*scale.back+center.back
    preddf=rbind(preddf,predVals)
    #为Canada.train增加一条新记录
    subdata=rbind(subdata,(GZMT[n+i,1:4]-center.back)/scale.back)
}
rownames(preddf)=NULL
subdata.test=GZMT[(N-30+1):N,1:4]
summary(as.vector(abs(preddf-subdata.test)*100/subdata.test))
## X600519.SS.Open   X600519.SS.High  X600519.SS.Low    X600519.SS.Close 
##  Min.   :0.02054   Min.   :0.1042   Min.   :0.01655   Min.   :0.09132  
##  1st Qu.:0.06233   1st Qu.:0.6119   1st Qu.:0.67018   1st Qu.:0.35635  
##  Median :0.48469   Median :1.0061   Median :0.89002   Median :1.50799  
##  Mean   :0.51659   Mean   :1.1806   Mean   :1.09273   Mean   :1.42640  
##  3rd Qu.:0.73470   3rd Qu.:1.6370   3rd Qu.:1.37965   3rd Qu.:2.28855  
##  Max.   :1.98097   Max.   :3.4275   Max.   :3.19283   Max.   :4.57756

par(mfrow=c(2,2))
plot(preddf[,1],type='l',ylab='Open')
lines(subdata.test[,1],lty=2,col='red')
plot(preddf[,2],type='l',ylab='High')
lines(subdata.test[,2],lty=2,col='red')
plot(preddf[,3],type='l',ylab='Low')
lines(subdata.test[,3],lty=2,col='red')
plot(preddf[,4],type='l',ylab='Close')
lines(subdata.test[,4],lty=2,col='red')
par(mfrow=c(1,1))

