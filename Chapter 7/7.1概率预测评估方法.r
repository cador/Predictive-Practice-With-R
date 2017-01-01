#自定义函数计算二元分类评估指标
#trueType:真实值，包括0和1分类值
#predType:预测值，包括0和1分类值
#return-->list(...)
ppe<-function(trueType,predType)
{
    #建立混淆矩阵
    confusionMatrix<-as.matrix(table(trueType,predType))
    #为TP、FN、FP、TN赋值
    TP<-confusionMatrix[2,2]
    FN<-confusionMatrix[2,1]
    FP<-confusionMatrix[1,2]
    TN<-confusionMatrix[1,1]
    #1.计算准确率（Accuracy，简记为A）
    e.A<-TP/(TP+FP)
    #2.计算负元准确率（Negtive Accuracy，简记为NA）
    e.NA<-TN/(TN+FN)
    #3.计算总体准确率（Total Accuracy，简记为TA）
    e.TA<-(TP+TN)/(TP+FN+FP+TN)
    #4.计算错误率（Error Rate，简记为ER）
    e.ER<-FP/(TP+FP)
    #5.计算负元错误率（Negtive Error Rate，简记为NER）
    e.NER<-FN/(FN+TN)
    #6.计算总体错误率（Total Error Rate，简记为TER）
    e.TER<-1-e.TA
    #7.计算覆盖率（Coverage Rate，简记为CR）
    e.CR<-TP/(TP+FN)
    #8.计算负元覆盖率（Negtive Coverage Rate，简记为NCR）
    e.NCR<-TN/(FP+TN)
    #9.计算假正率（FP Rate，简记为FPR）
    e.FPR<-FP/(FP+TN)
    #10.计算假负率（FN Rate，简记为FNR）
    e.FNR<-FN/(TP+FN)
    #11.计算F值
    e.F<-2*e.A*e.CR/(e.A+e.CR)
    #12.计算提升度（Lift Value，简记为LV）
    e.LV<-e.A/((TP+FN)/(TP+FN+FP+TN))
    #13.计算?相关系数
    e.phi<-(TP*TN-FP*FN)/sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN))
    #14.计算Kappa系数
    pe<-((TP+FN)*(TP+FP)+(FP+TN)*(FN+TN))/(TP+FN+FP+TN)^2
    e.Kappa<-(e.TA-pe)/(1-pe)
    return(list(e.A=e.A,e.NA=e.NA,e.TA=e.TA,e.ER=e.ER,e.NER=e.NER,e.TER=e.TER,
        e.CR=e.CR,e.NCR=e.NCR,e.FPR=e.FPR,e.FNR=e.FNR,e.F=e.F,e.LV=e.LV,
        e.phi=e.phi,e.Kappa=e.Kappa))
}

library(randomForest)
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
#对iris进行预处理，得到用于建立二分类模型的基础数据集
d0=iris[as.character(iris$Species)=='setosa' | 
        as.character(iris$Species)=='virginica',]
d0.species=as.character(d0$Species)
d0.species[d0.species=='setosa']=1
d0.species[d0.species=='virginica']=0
d0$Species=factor(d0.species,levels=c(0,1))
#调用rpart函数，建立分类回归模型
rf=randomForest(Species~.,data=d0)

#使用随机扰动的方法，更新d0
tmp0<-as.matrix(d0[,1:4])+matrix(runif(nrow(d0)*(ncol(d0)-1),-2,2),ncol=4)
tmp0[tmp0<=0]<-min(d0[,1:4])
d1=data.frame(tmp0,Species=d0$Species)
d1$pred=predict(rf,newdata=d1,type="prob")
head(d1)
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species pred.0 pred.1
## 1     4.469148    3.504227    1.3040429   0.9930959       1  0.106  0.894
## 2     6.423319    1.751841    3.0207776   0.1000000       1  0.094  0.906
## 3     4.177635    1.665832    0.4778200   1.1534713       1  0.452  0.548
## 4     4.742109    3.285896    3.2269695   0.1000000       1  0.302  0.698
## 5     6.960892    4.219651    0.3813859   1.3764016       1  0.512  0.488
## 6     4.533124    4.460552    2.9422134   0.4898234       1  0.000  1.000

#定义函数绘制ROC曲线
#prob：预测结果为正的概率向量
#labels：实际结果分类向量
rroc<-function(prob,labels)
{
    #构建rocsimple数据框
    rocsimple<-data.frame(pred=prob,label=labels)
    #对rocsimple的结果，按概率，从大到小排序
    rocsimple<-rocsimple[order(rocsimple$pred,decreasing=T),]
    rownames(rocsimple)=NULL
    #依次遍历每行，并计算对应的假正率和真正率。若数据量太大，可将区间等分
    fpr<-0
    tpr<-0
    for(i in 1:nrow(rocsimple))
    {
        t0<-rocsimple[i,1]
        rocsimple$predLabel<-factor(0,levels=c(0,1))
        rocsimple[rocsimple$pred>=t0,]$predLabel<-1
        confusionMatrix<-as.matrix(table(rocsimple[,2:3]))
        fpr<-c(fpr,confusionMatrix[1,2]/sum(confusionMatrix[1,]))
        tpr<-c(tpr,confusionMatrix[2,2]/sum(confusionMatrix[2,]))
    }
    plot(fpr,tpr,col='white',xlab="False positive rate",
         ylab="True positive rate")
    lines(fpr,tpr)
    return(list(vfpr=fpr,vtpr=tpr))
}
roc.plot<-rroc(d1$pred[,2],d1$Species)

#计算AUC
auc<-function(vfpr,vtpr,N=100,alpha=0.5)
{
    #将横轴、纵轴等分为N份
    x<-(1:N)/N
    y<-x
    mount<-0
    for(i in 1:N)
    {
        #计算当前值x[i]离fpr最近的位置，然后对该位置对应的tpr的值
        val=vtpr[which.min(abs(x[i]-vfpr))]
        #计算y向量中小于等于val的元素个数
        mount<-mount+NROW(y[y<=val])
    }
    aucval<-(mount+alpha*N)/(N*N)
    return(aucval)
}
auc(roc.plot$vfpr,roc.plot$vtpr)
## [1] 0.9056

library(ROCR)
pred <- prediction(d1$pred[,2], d1$Species)
#设置参数，横轴为假正率fpr，纵轴为真正率tpr
perf <- performance(pred,"tpr","fpr")
#绘制ROC曲线
plot(perf)

#计算AUC的值
auc.obj <- performance(pred,"auc")
auc <- auc.obj@y.values[[1]]
auc
## [1] 0.9022

#自定义函数绘制KS曲线
#prob：预测结果为正的概率向量
#labels：实际结果分类向量
#n:概率区间等分份数，即n分位数
#return
#ks.value:KS值
ks.plot<-function(prob,labels,n=100)
{
    L<-NROW(prob)
    if(n>L){n<-L}
    #构造data.frame
    tmpdata<-data.frame(prob,labels)
    #将prob按降序方式排序 
    tmpdata<-tmpdata[order(tmpdata$prob,decreasing=T),]
    tmpdata$rowno=1:L
    #将概率区间等比例分成n份
    qus<-quantile(1:L,probs=seq(0,1,1/n))
    culList1<-culList0<-NULL
    out<-mapply(function(i){
        sublab<-tmpdata[tmpdata$rowno>=1 & 
                        tmpdata$rowno<ifelse(i==n,qus[i+1]+0.001,qus[i+1]),]
        culList1<<-c(culList1,sum(sublab$labels==1))
        culList0<<-c(culList0,sum(sublab$labels==0)) 
    },1:n)
    culList1<-culList1/sum(labels==1)
    culList0<-culList0/sum(labels==0)
    #开始绘制KS曲线
    plot(1:n,culList1,col='white',xlab=paste(n,"分位数",sep=""),ylab="累计占比",
         xlim=c(0,n),ylim=c(0,1))
    lines(1:n,culList1,col='blue',lwd=2)
    lines(1:n,culList0,col='red',lwd=2)
    legend(0,1,legend=c("累计占比-1","累计占比-0"),col=c("blue","red"),lty=2)
    #计算KS值，并在图形中标出
    ks.value<-max(culList1-culList0)
    x<-(1:n)[which.max(culList1-culList0)]
    abline(v=x,lty=2,col='gray',lwd=2)
    return(ks.value)
}
ks.plot(d1$pred[,2],d1$Species)

#自定义函数绘制累计收益图
#prob：预测结果为正的概率向量
#labels：实际结果分类向量
#n:概率区间等分份数，即n分位数
gain.plot<-function(prob,labels,n=100)
{
    L<-NROW(prob)
    if(n>L){n<-L}
    #构造data.frame
    tmpdata<-data.frame(prob,labels)
    #将prob按降序方式排序 
    tmpdata<-tmpdata[order(tmpdata$prob,decreasing=T),]
    tmpdata$rowno=1:L
    #将概率区间等比例分成n份
    qus<-quantile(1:L,probs=seq(0,1,1/n))
    culList1<-NULL
    out<-mapply(function(i){
        sublab<-tmpdata[tmpdata$rowno>=1 & 
                        tmpdata$rowno<ifelse(i==n,qus[i+1]+0.001,qus[i+1]),]
        culList1<<-c(culList1,sum(sublab$labels==1)) 
    },1:n)
    culList1<-culList1*100/sum(labels==1)
    #开始绘制累计收益图
    plot(1:n,culList1,col='white',xlab=paste(n,"分位数",sep=""),ylab="%累计增益",
         ylim=c(0,100),xlim=c(0,n))
    lines(1:n,culList1,col='blue',lwd=2)
    abline(0,100/n,col='gray',lwd=2,lty=2)
}
gain.plot(d1$pred[,2],d1$Species)

#自定义函数绘制累计提升图
#prob：预测结果为正的概率向量
#labels：实际结果分类向量
#n:概率区间等分份数，即n分位数
lift.plot<-function(prob,labels,n=100)
{
    L<-NROW(prob)
    if(n>L){n<-L}
    #构造data.frame
    tmpdata<-data.frame(prob,labels)
    #将prob按降序方式排序 
    tmpdata<-tmpdata[order(tmpdata$prob,decreasing=T),]
    tmpdata$rowno=1:L
    #将概率区间等比例分成n份
    qus<-quantile(1:L,probs=seq(0,1,1/n))
    culList1<-NULL
    out<-mapply(function(i){
        sublab<-tmpdata[tmpdata$rowno>=1 & 
                        tmpdata$rowno<ifelse(i==n,qus[i+1]+0.001,qus[i+1]),]
        culList1<<-c(culList1,sum(sublab$labels==1)/NROW(sublab))  
    },1:n)
    culList1<-culList1*L/sum(labels==1)
    #开始绘制累计提升图
    plot(1:n,culList1,col='white',xlab=paste(n,"分位数",sep=""),
        ylab="累计提升度",xlim=c(0,n))
    lines(1:n,culList1,col='blue',lwd=2)
    abline(h=1,col='gray',lwd=2,lty=2)
}
lift.plot(d1$pred[,2],d1$Species)

#自定义函数绘制累计响应图
#prob：预测结果为正的概率向量
#vlabels：实际结果分类向量
#n:概率区间等分份数，即n分位数
response.plot<-function(prob,vlabels,n=100)
{
    L<-NROW(prob)
    if(n>L){n<-L}
    #构造data.frame
    tmpdata<-data.frame(prob,vlabels)
    #将prob按降序方式排序 
    tmpdata<-tmpdata[order(tmpdata$prob,decreasing=T),]
    tmpdata$rowno=1:L
    #将概率区间等比例分成n份
    qus<-quantile(1:L,probs=seq(0,1,1/n))
    culList1<-NULL
    out<-mapply(function(i){
        sublab<-tmpdata[tmpdata$rowno>=1 & 
            tmpdata$rowno<ifelse(i==n,qus[i+1]+0.001,qus[i+1]),]
            culList1<<-c(culList1,sum(sublab$vlabels==1)*100/NROW(sublab))      
    },1:n)
    #开始绘制累计响应图
    plot(1:n,culList1,col='white',xlab=paste(n,"分位数",sep=""),
        ylab="%累计响应率",xlim=c(0,n))
    lines(1:n,culList1,col='blue',lwd=2)
}
response.plot(d1$pred[,2],d1$Species)

