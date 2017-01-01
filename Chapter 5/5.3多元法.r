summary(lm(Y~X1+X2+X4,data=cemht))
## 
## Call:
## lm(formula = Y ~ X1 + X2 + X4, data = cemht)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.0886 -1.7944  0.2582  1.2888  3.9105 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  71.5838    14.1993   5.041 0.000698 ***
## X1            1.4513     0.1175  12.355 6.01e-07 ***
## X2            0.4168     0.1864   2.237 0.052119 .  
## X4           -0.2356     0.1740  -1.354 0.208634    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.318 on 9 degrees of freedom
## Multiple R-squared:  0.9822, Adjusted R-squared:  0.9762 
## F-statistic: 165.4 on 3 and 9 DF,  p-value: 3.456e-08

summary(lm(Y~X1+X3+X4,data=cemht))
## 
## Call:
## lm(formula = Y ~ X1 + X3 + X4, data = cemht)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.9364 -1.7932  0.4937  1.1389  3.7891 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 111.65815    4.58810  24.336 1.60e-09 ***
## X1            1.05192    0.22494   4.676  0.00116 ** 
## X3           -0.40925    0.20035  -2.043  0.07145 .  
## X4           -0.64250    0.04479 -14.343 1.66e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.39 on 9 degrees of freedom
## Multiple R-squared:  0.9811, Adjusted R-squared:  0.9747 
## F-statistic: 155.4 on 3 and 9 DF,  p-value: 4.548e-08

summary(lm(Y~X1+X2,data=cemht))
## 
## Call:
## lm(formula = Y ~ X1 + X2, data = cemht)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.880 -1.603 -1.293  1.380  4.059 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 52.58506    2.29232   22.94 5.59e-10 ***
## X1           1.46758    0.12163   12.07 2.77e-07 ***
## X2           0.66204    0.04598   14.40 5.18e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.413 on 10 degrees of freedom
## Multiple R-squared:  0.9785, Adjusted R-squared:  0.9743 
## F-statistic: 228.1 on 2 and 10 DF,  p-value: 4.541e-09

cemht=read.csv("f:\\cemht.csv",header=T)
cemht$X=NULL
lm.fit=lm(Y~X1+X2+X3+X4,data=cemht)
lm.fit.step=step(lm.fit)
## Start:  AIC=27.04
## Y ~ X1 + X2 + X3 + X4
## 
##        Df Sum of Sq    RSS    AIC
## - X3    1    0.1575 48.360 25.078
## - X4    1    0.1845 48.387 25.085
## - X2    1    3.2064 51.408 25.873
## <none>              48.202 27.036
## - X1    1   26.6020 74.804 30.749
## 
## Step:  AIC=25.08
## Y ~ X1 + X2 + X4
## 
##        Df Sum of Sq    RSS    AIC
## <none>               48.36 25.078
## - X4    1      9.86  58.22 25.490
## - X2    1     26.88  75.24 28.825
## - X1    1    820.16 868.52 60.624

summary(lm.fit.step)
## 
## Call:
## lm(formula = Y ~ X1 + X2 + X4, data = cemht)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.0886 -1.7944  0.2582  1.2888  3.9105 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  71.5838    14.1993   5.041 0.000698 ***
## X1            1.4513     0.1175  12.355 6.01e-07 ***
## X2            0.4168     0.1864   2.237 0.052119 .  
## X4           -0.2356     0.1740  -1.354 0.208634    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.318 on 9 degrees of freedom
## Multiple R-squared:  0.9822, Adjusted R-squared:  0.9762 
## F-statistic: 165.4 on 3 and 9 DF,  p-value: 3.456e-08


library(rpart)
#随机生成500棵树，并计算各输入变量的重要性得分
vilist=NULL
for(r in 1:500)
{
    #有放回抽取nrow(iris)行
    selrows=sample(1:nrow(iris),nrow(iris),replace=T)
    #无放回抽取2个变量
    selcols=c("Species",sample(setdiff(colnames(iris),"Species"),2))
    #构建训练集
    trainset=iris[selrows,selcols]
    #获得OOB数据集
    oobset=iris[setdiff(1:nrow(iris),selrows),selcols]
    #以Species为响应变量，建立分类回归树（这里是分类树）
    t0=rpart(Species~.,data=trainset)
    #对OOB数据集进行预测
    oobset$pred=predict(t0,oobset,type="class")
    #计算误差率
    oob=1-sum(diag(table(oobset$Species,oobset$pred)))/nrow(oobset)
    #去除pred属性
    oobset$pred=NULL
    #设置输入变量
    var=setdiff(selcols,"Species")
    #声明该决策树每个变量的decreaseAccuracy向量
    decreaseAccuracy=NULL
    for(ivar in var)
    {
        oobset_sl=oobset
        #使用随机的方法，将ivar对应的变量打乱
        eval(parse(text=paste("oobset_sl$",ivar,"=sample(oobset_sl$",ivar,
                              ",nrow(oobset_sl))",sep="")))
        #对ivar变量打乱后的OOB数据集进行预测
        oobset_sl$pred=predict(t0,oobset_sl,type="class")
        #计算误差率
        oob_sl=1-sum(diag(table(oobset_sl$Species,
                                oobset_sl$pred)))/nrow(oobset_sl)
        #计算精度损失大小
        decreaseAccuracy=c(decreaseAccuracy,(oob_sl-oob))
    }
    #对每个值赋值对应的变量名
    names(decreaseAccuracy)=var
    #将该决策树的decreaseAccuracy向量追加到vilist中
    vilist=c(vilist,decreaseAccuracy)
}
#分别以names(vilist)、vilist为向量构建数据框
d0=data.frame(var=names(vilist),val=vilist,stringsAsFactors=F)
#分组统计精度损失大小的平均值
out_avg=aggregate.data.frame(d0$val,by=list(d0$var),mean)
#分组统计精度损失大小的标准差
out_sd=aggregate.data.frame(d0$val,by=list(d0$var),sd)
#计算没有标准化的MeanDecreaseAccuracy指标
MeanDecreaseAccuracy=out_avg[,2]
names(MeanDecreaseAccuracy)=out_avg[,1]
print(MeanDecreaseAccuracy)
## Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
##   0.49270153   0.51811290   0.10675872   0.02863008
#计算经过标准化的MeanDecreaseAccuracy指标
MeanDecreaseAccuracy.scale=out_avg[,2]/out_sd[,2]
names(MeanDecreaseAccuracy.scale)=out_sd[,1]
print(MeanDecreaseAccuracy.scale)
## Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
##    2.3670413    2.7840887    0.7002987    0.4799055


#随机生成500棵树，并计算各输入变量的重要性得分
vilist=NULL
for(r in 1:500)
{
    #有放回抽取nrow(iris)行
    selrows=sample(1:nrow(iris),nrow(iris),replace=T)
    #无放回抽取2个变量
    selcols=c("Petal.Width",sample(setdiff(colnames(iris),"Petal.Width"),2))
    #构建训练集
    trainset=iris[selrows,selcols]
    #获得OOB数据集
    oobset=iris[setdiff(1:nrow(iris),selrows),selcols]
    #以Petal.Width为响应变量，建立分类回归树（这里是回归树）
    t0=rpart(Petal.Width~.,data=trainset)
    #对OOB数据集进行预测
    oobset$pred=predict(t0,oobset)
    #计算均方误差
    oob=mean((oobset$Petal.Width-oobset$pred)^2)
    #去除pred属性
    oobset$pred=NULL
    #设置输入变量
    var=setdiff(selcols,"Petal.Width")
    #声明该决策树每个变量的mse向量
    mse=NULL
    for(ivar in var)
    {
        oobset_sl=oobset
        #使用随机的方法，将ivar对应的变量打乱
        eval(parse(text=paste("oobset_sl$",ivar,"=sample(oobset_sl$",ivar,
                              ",nrow(oobset_sl))",sep="")))
        #对ivar变量打乱后的OOB数据集进行预测
        oobset_sl$pred=predict(t0,oobset_sl)
        #计算均方误差
        oob_sl=mean((oobset_sl$Petal.Width-oobset_sl$pred)^2)
        #计算MSE的增长百分率
        mse=c(mse,(oob_sl-oob)/oob)
    }
    #对每个值赋值对应的变量名
    names(mse)=var
    #将该决策树的mse向量追加到vilist中
    vilist=c(vilist,mse)
}
#分别以names(vilist)、vilist为向量构建数据框
d0=data.frame(var=names(vilist),val=vilist,stringsAsFactors=F)
#分组统计MSE的增长百分率的平均值
out_avg=aggregate.data.frame(d0$val,by=list(d0$var),mean)
#分组统计MSE的增长百分率的标准差
out_sd=aggregate.data.frame(d0$val,by=list(d0$var),sd)
#计算没有标准化的IncMSE指标
IncMSE=out_avg[,2]
names(IncMSE)=out_avg[,1]
print(IncMSE)
## Petal.Length Sepal.Length  Sepal.Width      Species 
##    20.073760     1.871118     0.455228    20.082866
#计算经过标准化的IncMSE指标
IncMSE.scale=out_avg[,2]/out_sd[,2]
names(IncMSE.scale)=out_sd[,1]
print(IncMSE.scale)
## Petal.Length Sepal.Length  Sepal.Width      Species 
##    2.0472571    0.6532487    0.9554329    1.6494987


#随机生成500棵树，并计算各输入变量的重要性得分
vilist=NULL
for(r in 1:500)
{
    #有放回抽取nrow(iris)行
    selrows=sample(1:nrow(iris),nrow(iris),replace=T)
    #无放回抽取2个变量
    selcols=c("Species",sample(setdiff(colnames(iris),"Species"),2))
    #构建训练集
    trainset=iris[selrows,selcols]
    #以Species为响应变量，建立分类回归树（这里是分类树）
    t0=rpart(Species~.,data=trainset)
    #将该决策树的variable.importance向量追加到vilist中
    vilist=c(vilist,t0$variable.importance)
}
#分别以names(vilist)、vilist为向量构建数据框
d0=data.frame(var=names(vilist),val=vilist,stringsAsFactors=F)
#分组统计variable.importance的平均值
out_avg=aggregate.data.frame(d0$val,by=list(d0$var),mean)
#计算MeanDecreaseGini指标
MeanDecreaseGini=out_avg[,2]
names(MeanDecreaseGini)=out_avg[,1]
print(MeanDecreaseGini)
## Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
##     88.32074     89.11598     53.51080     33.69596

#随机生成500棵树，并计算各输入变量的重要性得分
vilist=NULL
for(r in 1:500)
{
    #有放回抽取nrow(iris)行
    selrows=sample(1:nrow(iris),nrow(iris),replace=T)
    #无放回抽取2个变量
    selcols=c("Petal.Width",sample(setdiff(colnames(iris),"Petal.Width"),2))
    #构建训练集
    trainset=iris[selrows,selcols]
    #以Petal.Width为响应变量，建立分类回归树（这里是回归树）
    t0=rpart(Petal.Width~.,data=trainset)
    #将该决策树的variable.importance向量追加到vilist中
    vilist=c(vilist,t0$variable.importance)
}
#分别以names(vilist)、vilist为向量构建数据框
d0=data.frame(var=names(vilist),val=vilist,stringsAsFactors=F)
#分组统计variable.importance的平均值
out_avg=aggregate.data.frame(d0$val,by=list(d0$var),mean)
#计算IncNodePurity指标
IncNodePurity=out_avg[,2]
names(IncNodePurity)=out_avg[,1]
print(IncNodePurity)
## Petal.Length Sepal.Length  Sepal.Width      Species 
##     80.39268     60.69096     35.34203     67.59255

library(randomForest)
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
rf=randomForest(Species~.,data=iris,importance=TRUE)
varImpPlot(rf,main="随机森林特征选择")

#也可以通过柱状图的方式来查看变量重要性的大小
par(mfrow=c(1,2))
x1=sort(importance(rf)[,4],decreasing=T)
readyCol=rainbow(9)
names(readyCol)=names(x1)
barplot(x1,col=readyCol,border='gray',main='MeanDecreaseAccuracy')
x2=sort(importance(rf)[,5],decreasing=T)
barplot(x2,col=readyCol[names(x2)],border='gray',main='MeanDecreaseGini')


library(genalg)
#自定义适应度函数，BIC越小，选用的特征越好
adjust<-function(x)
{
    tmp=d0[,c(5,which(x>0))]
    if(class(tmp)!="numeric")
    {
        lmstr=paste("lm.fit=lm(Y~",paste(setdiff(names(tmp),"Y"),collapse="+"),
                                         ",data=tmp)",sep="")
        eval(parse(text=lmstr))
        return(BIC(lm.fit))
    }else{
        return(exp(100))
    }
}
rg0<-rbga.bin(size=4,popSize=50,iters=100,elitism=5,evalFunc=adjust,
              mutationChance=0.01,zeroToOneRatio=10)
plot(rg0)

head(rg0$population)
##      [,1] [,2] [,3] [,4]
## [1,]    1    1    0    0
## [2,]    1    1    0    0
## [3,]    1    1    0    0
## [4,]    1    1    0    0
## [5,]    1    1    0    0
## [6,]    1    1    0    0

