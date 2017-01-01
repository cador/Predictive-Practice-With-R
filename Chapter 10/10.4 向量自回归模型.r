#加载vars包，并加载Canada数据
library(vars)
data("Canada")
#去掉Canada后面8条记录（验证数据），提取训练数据
Canada.train=Canada[1:76,]
#由于后面需要对误差进行累加，此处对所有指标，按均值和标准差进行标准化处理
Canada.train=scale(Canada.train)
center.back=attr(Canada.train,"scaled:center")
scale.back=attr(Canada.train,"scaled:scale")
#加载fUnitRoots，使用adfTest进行单位根检验
library(fUnitRoots)
for(i in 1:ncol(Canada.train))
{
    pValue=adfTest(Canada.train[,i])@test$p.value
    print(paste("指标",colnames(Canada.train)[i],"单位根检验的p值为：",pValue))
}
## [1] "指标 e  单位根检验的p值为： 0.396863105180591"
## [1] "指标 prod  单位根检验的p值为： 0.390827146894632"
## [1] "指标 rw  单位根检验的p值为： 0.080951239474415"
## [1] "指标 U  单位根检验的p值为： 0.0131210210458432"

#由于这四个指标都不平稳，因此需要进行合适的差分运算
library(timeSeries)
Canada.train.diff=Canada.train[1:75,]
for(i in 1:ncol(Canada.train))
{
    v0=diff(Canada.train[,i])
    Canada.train.diff[,i]=v0
    pValue=adfTest(v0)@test$p.value
    print(paste("指标",colnames(Canada.train)[i],"单位根检验的p值为：",pValue))
}
## [1] "指标 e  单位根检验的p值为： 0.01"
## [1] "指标 prod  单位根检验的p值为： 0.01"
## [1] "指标 rw  单位根检验的p值为： 0.01"
## [1] "指标 U  单位根检验的p值为： 0.01"

#模型阶数从1开始逐一增加
rowCol=dim(Canada.train.diff)
aicList=NULL
lmList=list()
for(p in 1:10)
{
    baseData=NULL
    for(i in (p+1):rowCol[1])
    {
        baseData=rbind(baseData,c(as.vector(Canada.train.diff[i,]),
                 as.vector(Canada.train.diff[(i-1):(i-p),])))
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
##     p    aicList
## 1   1 -19.159702
## 2   2 -16.778361
## 3   3  -8.570213
## 4   4   7.744633
## 5   5  35.689341
## 6   6  78.457497
## 7   7 139.219904
## 8   8 221.508895
## 9   9 329.671812
## 10 10 467.652561

p=which.min(aicList)
n=nrow(Canada.train.diff)
preddf=NULL
for(i in 1:8)
{
    predData=as.vector(Canada.train.diff[(n+i-1):(n+i-p),])
    predVals=c(1,predData)%*%lmList[[p]]
    #使用逆差分运算，还原预测值
    predVals=Canada.train[n+i,]+predVals
    #使用均值和标准差，还原预测值
    predVals=predVals*scale.back+center.back
    preddf=rbind(preddf,predVals)
    #为Canada.train增加一条新记录
    Canada.train=rbind(Canada.train,(Canada[n+i+1,]-center.back)/scale.back)
    #为Canada.train.diff增加一条新记录
    Canada.train.diff=rbind(Canada.train.diff,
                      Canada.train[n+i+1,]-Canada.train[n+i,])
}
rownames(preddf)=NULL
Canada.test=Canada[77:84,]
#分析预测残差情况
preddf-Canada.test
##                e        prod          rw          U
## [1,]  0.20065726 -0.72082735  0.08095581 -0.1872566
## [2,]  0.03650848 -0.08061884  0.05900702 -0.2266762
## [3,]  0.03751551 -0.87174188  0.17291547  0.1038101
## [4,] -0.04826465 -0.06498829  0.45879444  0.3488549
## [5,] -0.15647972 -0.60962289 -1.12199430 -0.1252027
## [6,]  0.51480507 -0.51864262  0.71239452 -0.2760806
## [7,]  0.32312144 -0.06077596 -0.14816928 -0.3992348
## [8,] -0.34031023  0.78080542  1.31294707  0.0177969
#统计预测百分误差率分布
summary(as.vector(abs(preddf-Canada.test)*100/Canada.test))
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.003811 0.018940 0.111100 0.799300 0.264200 5.761000 

startN=60
seloc=(78-startN):(85-startN)
subdata=Canada[startN:84,]
par(mfrow=c(2,2))
for(i in 1:ncol(Canada))
{
    plot(subdata[,i],type='l',lwd=2,col='gray',ylim=range(Canada[,i]))
    lines(seloc,preddf[,i],col='red',lty=3,lwd=2)
}
par(mfrow=c(1,1))

n=nrow(Canada.train.diff)
preddf=NULL
for(i in 1:8)
{
    var.obj=VAR(Canada.train.diff,lag.max=10)
    print(var.obj$p)
    preds=predict(var.obj,n.ahead=1)$fcst
    predVals=mapply(function(x)x,preds)[1,]
    #使用逆差分运算，还原预测值
    predVals=Canada.train[n+i,]+predVals
    #使用均值和标准差，还原预测值
    predVals=predVals*scale.back+center.back
    preddf=rbind(preddf,predVals)
    #为Canada.train增加一条新记录
    Canada.train=rbind(Canada.train,(Canada[n+i+1,]-center.back)/scale.back)
    #为Canada.train.diff增加一条新记录
    Canada.train.diff=rbind(Canada.train.diff,
                      Canada.train[n+i+1,]-Canada.train[n+i,])
}
## AIC(n) AIC(n) AIC(n) AIC(n) AIC(n) AIC(n) AIC(n) AIC(n) 
##      1     1      1      1      1      1      1      1 
rownames(preddf)=NULL
Canada.test=Canada[77:84,]
#分析预测残差情况
preddf-Canada.test
## ......略
#统计预测百分误差率分布
summary(as.vector(abs(preddf-Canada.test)*100/Canada.test))
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000123 0.017150 0.104000 0.810100 0.331600 5.643000 

