vdata=read.table("f:\\sunspot-1700-2010.txt",header=F)
dim(vdata)
## [1] 311   2
#使用未来50年的数据进行验证，以前的数据用于建模
modelData=vdata$V2[1:261]
n<-length(modelData)
#设置候选门限值
thresholdV=sort(modelData)[round((seq(from=30,to=70,by=3)/100)*n)]
#设置最大门限延迟量dmax、自回归最大阶数、默认最小AIC值
dmax=5
pmax=5
minAIC=1e+10

#在指定门限延迟量、阶数及门限值的前提下，返回对应自回归模型AIC值和自回归系数
getModelInfo<-function(tsobj,d,p,r,isUp=TRUE)
{
    if(isUp){
        dstSet=which(tsobj>r)+d
    }else{
        dstSet=which(tsobj<=r)+d
    }
    tmpdata=NULL
    #重建基础数据集
    #xt=a0+a1*x(t-1)+...+ap*x(t-p)
    for(i in dstSet)
    {
        if(i>p && i<length(tsobj))
        {
            tmpdata=rbind(tmpdata,tsobj[i:(i-p)])
        }
    }
    x=cbind(1,tmpdata[,2:(p+1)])
    coef=solve(t(x)%*%x)%*%t(x)%*%tmpdata[,1]
    epsilon=tmpdata[,1]-x%*%coef
    aic=drop(nrow(tmpdata)*log(var(epsilon))+2*(p+2))
    return(list(aic=aic,coef=coef))
}

#选择最优参数
for(tsv in thresholdV)
{
    for(d in 1:dmax)
    {
        for(p1 in 1:pmax) #<=r1
        {
            model1=getModelInfo(modelData,d,p=p1,r=tsv,isUp=F)
            for(p2 in 1:pmax) #>r1
            {
                model2=getModelInfo(modelData,d,p=p2,r=tsv,isUp=T)
                if((model1$aic+model2$aic)<minAIC)
                {
                    minAIC=model1$aic+model2$aic
                    a_tsv=tsv
                    a_d=d
                    a_p1=p1
                    a_p2=p2
                    coef1=model1$coef
                    coef2=model2$coef
                    print(minAIC)
                }
            }
        }
    }
}
## [1] 1569.373
## [1] 1448.262
## [1] 1445.631
## [1] ......
## [1] 1320.53
## [1] 1319.682

##########使用求出的参数，对未来50年的数据逐年预测
predsData=NULL
for(i in 262:311)
{
    t0=vdata$V2[(i-a_d)]
    if(t0<=a_tsv)
    {
        predsData=c(predsData,sum(c(1,vdata$V2[(i-1):(i-a_p1)])*coef1))
    }else{
        predsData=c(predsData,sum(c(1,vdata$V2[(i-1):(i-a_p2)])*coef2))
    }
}
plot(vdata$V2[200:311],type='l',col='darkgreen',lwd=2)
lines(63:112,predsData,col='red',lwd=2,lty=2)
legend(0,180,c("真实值","预测值"),col=c("darkgreen","red"),lty=1,lwd=2)


library(TSA)
vdata=read.table("f:\\sunspot-1700-2010.txt",header=F)
dim(vdata)
#使用未来50年的数据进行验证，以前的数据用于建模
predsData=NULL
for(i in 1:50)
{
    modelData=vdata$V2[1:(261+i-1)]
    tar.obj=tar(y=modelData,p1=5,p2=3,d=3,a=.3,b=.7)
    predsData=c(predsData,predict(tar.obj,n.ahead=1)$fit)
}
plot(vdata$V2[200:311],type='l',col='darkgreen',lwd=2)
lines(63:112,predsData,col='red',lwd=2,lty=2)
legend(0,180,c("真实值","预测值"),col=c("darkgreen","red"),lty=1,lwd=2)


