#建立函数构建GBRT模型
#y:响应变量
#x:输入变量数据框
#consame:当连续consame次得到的残差平方和相等时算法终止
#maxiter:迭代次数的上限
#lambda:缩放因子
gbrt.build<-function(y,x,consame=5,maxiter=1000,lambda=0.01)
{
    #加载实现二叉回归树的包
    library(rpart)
    #使平方损失函数最小化的常数值为对应数据的平均值，即以均值初始化f0
    f0<-mean(y)
    #初始化变量
    rss<-NULL
    gbrt.model.list<-list(f0=f0)
    #进入循环，当连续consame次，得到的残差平方和相等或超过最大迭代次数时终止算法
    for(m in 1:maxiter)
    {
        #计算负梯度，当损失函数为平方损失函数时，负梯度即为残差
        revals<-y-f0
        #根据残差学习一棵回归树，设置分割点满足的最小样本量为10
        rpart.mod<-rpart(
            formula(paste("revals~",paste(colnames(x),collapse="+"),sep="")),
            data=x,control=rpart.control(minsplit=10))
        #更新回归树，并生成估计结果
        gbrt.model.list=append(gbrt.model.list,list(rpart.mod))
        names(gbrt.model.list)[m+1]=paste("f",m,sep="")
        f0=f0+lambda*predict(rpart.mod,x)
        #统计残差平方和
        rss=c(rss,sum((f0-y)^2))
        #判断是否满足终止条件
        n<-length(rss)
        if(n>=consame && sd(rss[(n-consame+1):n])==0)break
    }
    return(list(m=m,rss=rss,gml=gbrt.model.list))
}

#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
out=gbrt.build(vdata$y,vdata[,1:3])
#查看rss的统计信息
summary(out$rss)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.362   1.688   3.189  11.700  12.200  84.960

#根据rss绘制曲线，以直观观察残差平方和的变化趋势
plot(out$rss,type='l',xlab="迭代次数",lwd=2,col='blue',ylab='RSS')
abline(h=0,col='red',lwd=2,lty=2)

#建立预测函数，对新数据进行预测
#newdata:进行预测的新数据
#gml:即gbrt.model.list，它是GBRT的模型
#lambda:训练模型时，指定的lamda参数
gbrt.predict<-function(newdata,gml,lambda=0.01)
{
    n=length(gml)
    f0=gml[[1]]
    for(k in 2:n)
    {
        f0=f0+lambda*predict(gml[[k]],newdata)
    }
    names(f0)=NULL
    return(f0)
}

newdata=vdata[,1:4]
newdata$pred=gbrt.predict(newdata,out$gml)
sum((newdata$y-newdata$pred)^2)
## [1] 1.362449

library(gbm)
#基础数据准备
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#建立gbm模型
gbm.obj<-gbm(y~x1+x2+x3,data=vdata,distribution='gaussian',
             var.monotone=c(0,0,0),
             n.trees=1000,
             shrinkage=0.01,
             interaction.depth=5,
             bag.fraction=0.5,
             cv.folds=10)
#用交叉验证确定最佳迭代次数
best.iter<-gbm.perf(gbm.obj,method="cv")
best.iter
## [1] 597

#进行预测
vdata$pred=predict(gbm.obj,vdata,n.trees=best.iter)
#查看前6行数据
head(vdata)
##    x1  x2  x3   y      pred
## 1 5.1 3.5 1.4 0.2 0.2653250
## 2 4.9 3.0 1.4 0.2 0.1903269
## 3 4.7 3.2 1.3 0.2 0.1804154
## 4 4.6 3.1 1.5 0.2 0.1831213
## 5 5.0 3.6 1.4 0.2 0.2462909
## 6 5.4 3.9 1.7 0.4 0.3657279
#统计残差平方和
sum((vdata$y-vdata$pred)^2)
## [1] 3.188981

#分析变量重要性
summary(gbm.obj,n.trees=best.iter)

#绘制各变量的边际图
par(mfrow=c(1,3))
plot(gbm.obj,1,best.iter)
plot(gbm.obj,2,best.iter)
plot(gbm.obj,3,best.iter)
par(mfrow=c(1,1))

