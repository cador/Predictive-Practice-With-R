#通过最近邻方法对缺失值进行修正
SeqKNN=function (data, k=10) 
{
    x <- as.matrix(data)
    N <- dim(x)
    p <- N[2]
    N <- N[1]
    nas <- is.na(drop(x %*% rep(1, p)))
    xcomplete <- x[!nas, ]
    xbad <- x[nas, , drop = FALSE]
    missing<-c()
    for (i in seq(nrow(xbad))) {
        missing[i]<-sum(is.na(xbad[i,]))
    }
    missingorder<-order(missing)
    xnas <- is.na(xbad)
    xbadhat <- xbad
    cat(nrow(xbad), fill = TRUE)
    for (i in seq(nrow(xbad))) {
        j<-order(missingorder[i])
        xinas <- xnas[missingorder[i], ]
        xbadhat[missingorder[i], ] <- nnmiss(xcomplete, 
            xbad[missingorder[i], ], xinas, K = k)
        xcomplete<-rbind(xcomplete, xbadhat[missingorder[i],]) 
    }
    x[nas, ] <- xbadhat
    return(data.frame(x))
}
nnmiss=function (x, xmiss, ismiss, K) 
{
    xd <- as.matrix(scale(x, xmiss, FALSE)[, !ismiss])
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    od <- order(dd)[seq(K)]
    od<-od[!is.na(od)]
    K<-length(od)
    distance<-dd[od]
    s<-sum(1/(distance+0.000000000000001))
    weight<-(1/(distance+0.000000000000001))/s
    xmiss[ismiss] <- drop(weight %*% x[od, ismiss, drop = FALSE]) 
    return(xmiss)
}

splinex=function(vc)
{
    x1=vc[1:(length(vc)-2)]
    x2=vc[2:(length(vc)-1)]
    x3=vc[3:(length(vc))]
    xint=(x1+x2+x3)/3
    out=c(vc[1],xint,vc[length(vc)])
    return(out)
}


#加载天气数据并对日期进行转换排序
weth=read.csv("C:/Users/haolin/Desktop/demo1.data/weth.csv",header=T)
weth$WETH_DATE=as.Date(weth$WETH_DATE)
weth=weth[order(weth$WETH_DATE),]
#加载时点负荷数据并对日期进行转换排序
data=read.csv("C:/Users/haolin/Desktop/demo1.data/data.csv",header=T)
data$LOAD_DATE=as.Date(data$LOAD_DATE)
data=data[order(data$LOAD_DATE),]
#对节假日数据进行处理
data[data$LOAD_DATE>="2013-06-10" & data$LOAD_DATE<="2013-06-12",2:97]=NA
data[data$LOAD_DATE>="2013-09-19" & data$LOAD_DATE<="2013-09-21",2:97]=NA
data[data$LOAD_DATE>="2013-10-01" & data$LOAD_DATE<="2013-10-07",2:97]=NA
data[data$LOAD_DATE=="2014-01-01",2:97]=NA
data[data$LOAD_DATE>="2014-01-31" & data$LOAD_DATE<="2014-02-06",2:97]=NA
data[data$LOAD_DATE>="2014-05-01" & data$LOAD_DATE<="2014-05-03",2:97]=NA
data[data$LOAD_DATE>="2014-05-31" & data$LOAD_DATE<="2014-06-02",2:97]=NA
#转换星期为整型值
data$week=as.integer(factor(weekdays(data$LOAD_DATE,abbreviate = T),
      levels=c("周一","周二","周三","周四","周五","周六","周日")))
#得到月份数据
data$MONTH=as.integer(sapply(as.character(data$LOAD_DATE),
       function(x){strsplit(x,"-")[[1]][2]}))
#增加趋势信息
data$TREND=1:nrow(data)
#构建日期索引，保证日期的完整性
dateDf=data.frame(IDATE=seq(from=as.Date('2013-06-01'),
       to=as.Date('2014-07-31'),by='1 day'))
#关联时点负荷数据与天气数据
library(sqldf)
out=sqldf("select * from dateDf left join weth on IDATE=WETH_DATE")
out=sqldf("select * from out left join data on IDATE=LOAD_DATE")
#去掉日期信息
out$IDATE=NULL
out$WETH_DATE=NULL
out$LOAD_DATE=NULL
#使用K近邻方法插补空值
vdata=SeqKNN(out)
#对插补得到的星期值可能是小数，这里进行四舍五入
vdata$week=round(vdata$week)


par(mfrow=c(2,2))
pacf(vdata$C010)
pacf(vdata$C040)
pacf(vdata$C060)
pacf(vdata$C080)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
ccf(vdata$C040,vdata$C010)
ccf(vdata$C040,vdata$C050)
ccf(vdata$C040,vdata$C060)
ccf(vdata$C040,vdata$C080)
par(mfrow=c(1,1))

plot(scale(vdata$C027),type='l',ylim=c(-3,2),col='darkgreen')
lines(scale(vdata$MEAN_TMP),col='red')

a0=aggregate(x=cbind(vdata$week,vdata$C040),by=list(vdata$week),mean)$V2
plot(a0,pch=20,ylim=c(100,140))
lines(a0)
a1=aggregate(x=cbind(vdata$week,vdata$C060),by=list(vdata$week),mean)$V2
points(a1,col='red',pch=20)
lines(a1,col='red')

#构建基础数据的R代码如下
weeks=1:6
names(weeks)=c("WEEK1","WEEK2","WEEK3","WEEK4","WEEK5","WEEK6")
vdata=cbind(vdata,mapply(function(x){as.integer(vdata$week==x)},weeks))
predDayData=cbind(LOAD_DATE=date.back,vdata[,c(1:3,5,105:110,103:104,6:101)])
N=nrow(predDayData)
lastData=predDayData[7:(N-1),c(2:5,14:109)]
colnames(lastData)=paste(names(lastData),"L1",sep="")
lastPeriodData=predDayData[1:(N-7),c(2:5,14:109)]
colnames(lastPeriodData)=paste(names(lastPeriodData),"L7",sep="")
predDayData=cbind(predDayData[8:N,],lastData,lastPeriodData)
#划分训练集和验证集
trainData=predDayData[predDayData$LOAD_DATE<="2014-06-30",]
validData=predDayData[predDayData$LOAD_DATE>"2014-06-30",]
rownames(trainData)=NULL
rownames(validData)=NULL
dim(trainData)
## [1] 388 309
dim(validData)
## [1]  31 309


#1、对训练输入数据进行标准化处理
trainData$LOAD_DATE=NULL
train.X=trainData[,c(1:12,109:308)]
train.Y=trainData[,c(13:108)]
scale.data=scale(train.X)
scale.data.center=attr(scale.data,"scaled:center")
scale.data.scale=attr(scale.data,"scaled:scale")
train.X=data.frame(scale.data)
#2、使用有偏抽样方法，从训练数据中划分训练集和测试集
rowids<-1:nrow(train.X)
sample.wts<-rowids^3/sum(rowids^3)
set.seed(973)
train.X.Validsamples=sample(rowids,60,prob=sample.wts)
hist(train.X.Validsamples,breaks=60)
train.X.Train=train.X[setdiff(rowids,train.X.Validsamples),]
train.Y.Train=train.Y[setdiff(rowids,train.X.Validsamples),]
train.X.Valid=train.X[train.X.Validsamples,]
train.Y.Valid=train.Y[train.X.Validsamples,]
rownames(train.X.Train)=NULL
rownames(train.Y.Train)=NULL
rownames(train.X.Valid)=NULL
rownames(train.Y.Valid)=NULL

#根据参数向量x，计算在测试集上的均方误差，该值越小，预测效果越好
getError<-function(x)
{
    #参数赋值
    lambda=x[1]
    alpha=x[2]
    #使用hclust函数，进行聚类分析
    train.X.Train.hclst<-hclust(dist(train.X.Train))
    train.X.Train$type=cutree(train.X.Train.hclst,h=lambda)
    #计算隐含层神经元个数p
    p=length(unique(train.X.Train$type))
    #......各类的中心分别为ci~p
    centers=aggregate(train.X.Train[,1:212],
        by=list(train.X.Train$type),FUN=mean)
    centers$Group.1=NULL
    centers=as.matrix(centers)
    #......计算各类的扩展常数σ(sigma)
    dst=as.matrix(dist(as.matrix(centers[,1:212])))
    dst[dst==0]=max(dst)
    sigmas=alpha*apply(dst,1,min)
    #构建隐含层神经元，得到Φ矩阵
    phiMatrix=NULL
    for(i in 1:nrow(train.X.Train))
    {
        xi=as.matrix(train.X.Train[i,1:212])
        tmp=t(matrix(rep(xi,p),ncol=p))
        phiMatrix=rbind(phiMatrix,
            diag(exp(-((tmp-centers)%*%t(tmp-centers))/(sigmas^2))))
    }
    phiMatrix=cbind(1,phiMatrix)
    colnames(phiMatrix)=NULL
    #求解权向量W为
    W=(solve(t(phiMatrix)%*%phiMatrix+diag(rep(1e-5,p+1)))%*%
           t(phiMatrix))%*%t(t(train.Y.Train))
    #计算测试集的phiMatrix
    phiMatrix=NULL
    for(i in 1:nrow(train.X.Valid))
    {
        xi=as.matrix(train.X.Valid[i,1:212])
        tmp=t(matrix(rep(xi,p),ncol=p))
        phiMatrix=rbind(phiMatrix,
            diag(exp(-((tmp-centers)%*%t(tmp-centers))/(sigmas^2))))
    }
    phiMatrix=cbind(1,phiMatrix)
    colnames(phiMatrix)=NULL
    fit.obj=phiMatrix%*%W
    #计算均方误差
    error=mean((train.Y.Valid-fit.obj)^2)
    print(paste("lambda:",lambda," alpha:",alpha," error:",error))
    return(error)
}

library(genalg)
#定义监控函数
monitor<-function(rbga0)
{
    #打印种群中第一个个体的值population[1,]
    print(rbga0$population[1,])
}
rbgaObj<-rbga(stringMin = c(1,1), stringMax = c(20,20), popSize = 100, 
              iters = 5, mutationChance = 0.01, monitorFunc = monitor, 
              evalFunc = getError, verbose = TRUE)
## Testing the sanity of parameters...
## Not showing GA settings...
## Starting with random values in the given domains...
## Starting iteration 1 
## Calucating evaluation values... 
## [1] "lambda: 8.76628550472  alpha: 10.3757122148  error: 2568.8082768"
## ...
##
## . done.
## Sending current state to rgba.monitor()...
## [1] 6.293012 12.250772
## Creating next generation...
##  sorting results...
##  applying elitism...
##  applying crossover...
##  applying mutations... 1 mutations applied
## Starting iteration 5 
## Calucating evaluation values... 
## [1] "lambda: 6.29305347292  alpha: 17.7674725295  error: 2641.7929633"
...
## [1] "lambda: 1.34301777374  alpha: 15.6875729208  error: 2497.14082143"
## . done.
## Sending current state to rgba.monitor()...
## [1]  6.293012 12.250772
argvc=rbgaObj$population[which.min(rbgaObj$evaluations),]
argvc
## [1]  6.293012 10.498772
getError(argvc)
## [1] "lambda: 6.29301205342  alpha: 10.498771764  error: 2424.98677419"
## [1] 2424.987

#1、对训练输入数据进行标准化处理
trainData$LOAD_DATE=NULL
train.X=trainData[,c(1:12,109:308)]
train.Y=trainData[,c(13:108)]
scale.train.y=scale(train.Y)
scale.train.y.center=attr(scale.train.y,"scaled:center")
scale.train.y.scale=attr(scale.train.y,"scaled:scale")
train.Y=data.frame(scale.train.y)
scale.data=scale(train.X)
scale.data.center=attr(scale.data,"scaled:center")
scale.data.scale=attr(scale.data,"scaled:scale")
train.X=data.frame(scale.data)
#2、使用hclust函数，进行聚类分析
lambda=6.293012
train.X.hclst<-hclust(dist(train.X))
train.X$type=cutree(train.X.hclst,h=lambda)
#3、隐含层神经元个数p为
p=length(unique(train.X$type))
p
## [1] 146
#......各类的中心分别为ci~p
centers=aggregate(train.X[,1:212],by=list(train.X$type),FUN=mean)
centers$Group.1=NULL
centers=as.matrix(centers)
#......各类的扩展常数为σ(sigma)
alpha=10.498772
dst=as.matrix(dist(as.matrix(centers[,1:212])))
dst[dst==0]=max(dst)
sigmas=alpha*apply(dst,1,min)
#4、构建隐含层神经元，得到Φ矩阵
phiMatrix=NULL
for(i in 1:nrow(train.X))
{
    xi=as.matrix(train.X[i,1:212])
    tmp=t(matrix(rep(xi,p),ncol=p))
    phiMatrix=rbind(phiMatrix,
        diag(exp(-((tmp-centers)%*%t(tmp-centers))/(sigmas^2))))
}
phiMatrix=cbind(1,phiMatrix)
colnames(phiMatrix)=NULL
#求解权向量W为
W=(solve(t(phiMatrix)%*%phiMatrix+diag(rep(1e-5,p+1)))%*%
        t(phiMatrix))%*%t(t(train.Y))
dim(W)
## [1] 51 96
W[1:10,1:5]
##             C001       C002        C003       C004       C005
##  [1,]  30.659808  34.399062  30.3819522  41.780021  40.599776
##  [2,]  -9.183343  -7.092188 -11.6068756  -2.118027  -5.684591
##  [3,]  22.373674  33.011079  15.4570749  22.058649  25.560801
##  [4,] -36.704746 -40.076530 -39.1614949 -49.426580 -48.435505
##  [5,]   3.474226  -9.606831  -0.8287129  -4.311182  -2.796912
##  [6,] -16.909833  -3.347591 -14.0432907  -4.609156  -7.579483
##  [7,]  43.113543  44.420626  27.7737501  26.170748  24.953867
##  [8,]  26.657200  21.389933  29.7991093  30.042627  27.190726
##  [9,] -51.932527 -50.772261 -48.6540003 -48.149052 -45.636225
## [10,] -34.099518 -41.681108 -41.1456549 -35.508286 -39.626888


#基于验证集进行预测
#对验证数据进行标准化处理
validN=nrow(validData)
validData.Y=validData[,14:109]
validData.X=(validData[,c(2:13,110:309)]-t(matrix(
    rep(scale.data.center,validN),ncol=validN)))/
    t(matrix(rep(scale.data.scale,validN),ncol=validN))
phiMatrix=NULL
for(i in 1:nrow(validData.X))
{
    xi=as.matrix(validData.X[i,1:212])
    tmp=t(matrix(rep(xi,p),ncol=p))
    phiMatrix=rbind(phiMatrix,
        diag(exp(-((tmp-centers)%*%t(tmp-centers))/(sigmas^2))))
}
phiMatrix=cbind(1,phiMatrix)
colnames(phiMatrix)=NULL
pred.obj=phiMatrix%*%W
pred.obj=t(apply(pred.obj,1,splinex))
pred.obj=pred.obj*t(matrix(rep(scale.train.y.scale,validN),ncol=validN))+
    t(matrix(rep(scale.train.y.center,validN),ncol=validN))
dim(pred.obj)
## [1] 31 96
pred.obj[1:10,1:5]
##           C001     C001     C002     C003     C004
##  [1,] 1217.036 1171.436 1130.990 1104.055 1080.711
##  [2,] 1141.020 1103.942 1070.507 1045.917 1025.489
##  [3,] 1279.827 1244.947 1206.492 1174.143 1141.639
##  [4,] 1308.429 1269.175 1230.916 1195.557 1166.533
##  [5,] 1384.885 1354.623 1325.757 1289.129 1266.517
##  [6,] 1502.072 1458.361 1443.470 1414.091 1416.860
##  [7,] 1415.336 1359.327 1336.430 1314.069 1315.687
##  [8,] 1118.422 1073.913 1039.998 1016.197 1001.761
##  [9,] 1154.617 1118.889 1084.876 1055.215 1026.595
## [10,] 1344.579 1305.668 1270.131 1238.717 1209.510


#通过绘图，观察预测的情况
diffsum=0
allsum=0
for(i in 1:validN)
{
    tiff(filename=paste("C:\\Users\\haolin\\Desktop\\pics\\",i,".tiff"))
    rag=range(c(pred.obj[i,],as.matrix(validData.Y[i,])[1,]))
    plot(as.matrix(validData.Y[i,])[1,],col='darkgreen',lwd=3,type='l')
    lines(pred.obj[i,],ylim=rag,col='red',lty=3,lwd=3)
    dev.off()
    diffsum=diffsum+sum(abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,]))
    allsum=allsum+sum(as.matrix(validData.Y[i,])[1,])
}
diffsum/allsum
## [1] 0.04720568


rates=NULL
for(i in 1:validN)
{
    rates=c(rates,abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,])/
        as.matrix(validData.Y[i,])[1,])
}
#绘制直方图
par(mfrow=c(2,1))
hist(rates,breaks=100)
hist(rates[rates<0.2],breaks=100)
par(mfrow=c(1,1))
#平均绝对百分误差
mean(rates)
## [1] 0.04790117


#训练集的行数和列数
n=nrow(train.X.Train)
nc=ncol(train.X.Train)
#将训练集的输入和输出分别转成矩阵x和y
x=as.matrix(train.X.Train)
y=as.matrix(train.Y.Train)
y=rbind(0,y)
#长度为n的单位列向量
I=t(t(rep(1,n)))
#将测试集的输入转成矩阵xvt
xvt=as.matrix(train.X.Valid)
#测试集的行数
nvt=nrow(train.X.Valid)
#根据参数向量x，计算在测试集上的均方误差，该值越小，预测效果越好
getError<-function(argx)
{
    sigma=argx[1]
    gama=argx[2]
    omiga=matrix(rep(0,n*n),ncol=n)
    for(i in 1:n)
    {
        xi=x[i,]
        deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=nc))^2
        omiga[i,]=exp(-rowSums(deltaX)/(sigma^2))
    }
    #构建矩阵A
    A=(omiga+(1/gama)*diag(n))
    A=cbind(I,A)
    A=rbind(c(0,t(I)),A)
    #求b和alpha参数
    b_alpha=solve(A)%*%y
    b=b_alpha[1,]
    alpha=b_alpha[-1,]
    #基于train.X.Valid进行预测
    ypred=NULL
    for(i in 1:nvt)
    {
        xvti=xvt[i,]
        deltaX=(x-matrix(rep(xvti,n),byrow=T,ncol=nc))^2
        ypred=rbind(ypred,exp(-rowSums(deltaX)/(sigma^2))%*%t(t(alpha))+b)
    }
    error=mean((train.Y.Valid-ypred)^2)
    print(paste("sigma:",sigma," gama:",gama," error:",error))
    return(error)
}


library(genalg)
#定义监控函数
monitor<-function(rbga0)
{
    #打印种群中第一个个体的值population[1,]
    print(rbga0$population[1,])
}
rbgaObj<-rbga(stringMin = c(1,1), stringMax = c(200,200), popSize = 100, 
              iters = 20, mutationChance = 0.01, monitorFunc = monitor, 
              evalFunc = getError, verbose = TRUE)
## ...省略
argvc=rbgaObj$population[which.min(rbgaObj$evaluations),]
argvc
## [1]  61.31397 177.76681
getError(argvc)
## [1] "sigma: 61.3139732107  gama: 177.766807683  error: 1177.18324696"
## [1] 1177.183
rbgaObj$iters
### [1] 20


sigma=argvc[1]
gama=argvc[2]
n=nrow(train.X)
nc=ncol(train.X)
x=as.matrix(train.X)
y=as.matrix(train.Y)
y=rbind(0,y)
I=t(t(rep(1,n)))
omiga=matrix(rep(0,n*n),ncol=n)
for(i in 1:n)
{
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=nc))^2
    omiga[i,]=exp(-rowSums(deltaX)/(sigma^2))
}
#构建矩阵A
A=(omiga+(1/gama)*diag(n))
A=cbind(I,A)
A=rbind(c(0,t(I)),A)
#求b和alpha参数
b_alpha=solve(A)%*%y
b=b_alpha[1,]
alpha=b_alpha[-1,]


#基于train.Y.Vliad进行预测
validN=nrow(validData)
validData.Y=validData[,14:109]
validData.X=(validData[,c(2:13,110:309)]-
       t(matrix(rep(scale.data.center,validN),ncol=validN)))/
       t(matrix(rep(scale.data.scale,validN),ncol=validN))
xvt=as.matrix(validData.X)
nvt=nrow(validData.X)
ypred=NULL
for(i in 1:nvt)
{
    xvti=xvt[i,]
    deltaX=(x-matrix(rep(xvti,n),byrow=T,ncol=nc))^2
    ypred=rbind(ypred,exp(-rowSums(deltaX)/(sigma^2))%*%t(t(alpha))+b)
}
pred.obj=ypred
pred.obj=t(apply(pred.obj,1,splinex))
dim(pred.obj)
## [1] 31 96
pred.obj[1:10,1:5]
##           C001     C001     C002     C003     C004
##  [1,] 1235.049 1189.400 1150.184 1119.498 1096.562
##  [2,] 1119.780 1081.406 1049.962 1026.025 1007.741
##  [3,] 1259.638 1221.976 1185.542 1153.286 1122.460
##  [4,] 1307.418 1272.606 1235.234 1197.697 1167.851
##  [5,] 1356.294 1329.207 1299.050 1258.542 1231.533
##  [6,] 1424.114 1395.630 1370.650 1332.767 1313.080
##  [7,] 1367.668 1330.766 1296.461 1262.627 1241.558
##  [8,] 1122.582 1081.842 1049.435 1029.158 1013.863
##  [9,] 1127.612 1098.236 1072.166 1047.283 1020.030
##  [10,] 1333.791 1301.405 1268.808 1227.717 1196.436


#通过绘图，观察预测的情况
diffsum=0
allsum=0
for(i in 1:validN)
{
    tiff(filename=paste("C:\\Users\\haolin\\Desktop\\pics\\",i,".tiff"))
    rag=range(c(pred.obj[i,],as.matrix(validData.Y[i,])[1,]))
    plot(as.matrix(validData.Y[i,])[1,],col='darkgreen',lwd=3,type='l')
    lines(pred.obj[i,],ylim=rag,col='red',lty=3,lwd=3)
    dev.off()
    diffsum=diffsum+sum(abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,]))
    allsum=allsum+sum(as.matrix(validData.Y[i,])[1,])
}
diffsum/allsum
## [1] 0.04515431


rates=NULL
for(i in 1:validN)
{
    rates=c(rates,abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,])/
        as.matrix(validData.Y[i,])[1,])
}
#绘制直方图
par(mfrow=c(2,1))
hist(rates,breaks=100)
hist(rates[rates<0.2],breaks=100)
par(mfrow=c(1,1))
#平均绝对百分误差
mean(rates)
## [1] 0.04504269

