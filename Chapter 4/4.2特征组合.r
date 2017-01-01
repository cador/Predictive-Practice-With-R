#基于熵离散法，获取分割点
#U：信源信号，分类向量或因子，通常指输出变量或目标变量
#V：接收信号，分类向量或因子，通常是输入变量或解释变量
getSplitVal<-function(U,V)
{
    #将V变量排序，并保存在变量sortedSL中
    sortedSL=sort(V)
    #循环：按顺序以sortedSL中的每个值作为分割点，重建新特征new，默认设置为0
    gainsV<-NULL
    splitV<-NULL
    for(i in 1:NROW(sortedSL))
    {
        splitVal=sortedSL[i]
        new=rep(0,NROW(V))
        if(sum(V>splitVal)>0)
        {
            new[V>splitVal]=1
        }
        gainsV<-c(gainsV,gains(U,new))
        splitV<-c(splitV,splitVal)
    }
    #分割点为
    finalSplitV<-splitV[which.max(gainsV)]
    return(finalSplitV)
}
getFeature<-function(vdata,idx1,idx2,support)
{
    orgNames<-colnames(vdata)
    vdata$X1<-(vdata[,idx1] & vdata[,idx2])
    vdata$X2<-(!vdata[,idx1] & vdata[,idx2])
    vdata$X3<-(vdata[,idx1] & !vdata[,idx2])
    vdata$X4<-(!vdata[,idx1] & !vdata[,idx2])
    if(sum(vdata$X1)<support || sum(vdata$X1)>(nrow(vdata)-support)){
        vdata$X1<-NULL
    }
    if(sum(vdata$X2)<support || sum(vdata$X2)>(nrow(vdata)-support)){
        vdata$X2<-NULL
    }
    if(sum(vdata$X3)<support || sum(vdata$X3)>(nrow(vdata)-support)){
        vdata$X3<-NULL
    }
    if(sum(vdata$X4)<support || sum(vdata$X4)>(nrow(vdata)-support)){
        vdata$X4<-NULL
    }
    return(vdata[,setdiff(colnames(vdata),orgNames)])
}

#1.首先需要将原始特征转换成逻辑特征
for(i in 1:4)
{
    splitVal=getSplitVal(iris$Species,iris[,i])
    new=rep(FALSE,nrow(iris))
    new[iris[,i]>splitVal]=TRUE
    iris[,i]=new
}
head(iris)
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1        FALSE        TRUE        FALSE       FALSE  setosa
## 2        FALSE       FALSE        FALSE       FALSE  setosa
## 3        FALSE       FALSE        FALSE       FALSE  setosa
## 4        FALSE       FALSE        FALSE       FALSE  setosa
## 5        FALSE        TRUE        FALSE       FALSE  setosa
## 6        FALSE        TRUE        FALSE       FALSE  setosa

#2.按不同特征取值，两两组合，以构建新特征，设置最小支持数20
support=40
library(gregmisc)
t0=apply(combinations(n=4,r=2),1,function(x){
    gf<-getFeature(iris,x[1],x[2],support)
    if(ncol(gf)>0 || class(gf)=="logical")
    {
        iris<<-cbind(iris,gf)
    }
})
colnames(iris)[6:ncol(iris)]=paste("X",1:(ncol(iris)-5),sep="")
head(iris)
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species    X1    X2
## 1        FALSE        TRUE        FALSE       FALSE  setosa FALSE FALSE
## 2        FALSE       FALSE        FALSE       FALSE  setosa FALSE FALSE
## 3        FALSE       FALSE        FALSE       FALSE  setosa FALSE FALSE
## 4        FALSE       FALSE        FALSE       FALSE  setosa FALSE FALSE
## 5        FALSE        TRUE        FALSE       FALSE  setosa FALSE FALSE
## 6        FALSE        TRUE        FALSE       FALSE  setosa FALSE FALSE
##     X3    X4    X5
## 1 TRUE FALSE FALSE
## 2 TRUE FALSE FALSE
## 3 TRUE FALSE FALSE
## 4 TRUE FALSE FALSE
## 5 TRUE FALSE FALSE
## 6 TRUE FALSE FALSE


#3.使用交叉验证检验，加入新特征，误差是否有所降低
library(rpart)
orgErr<-NULL
newErr<-NULL
for(i in 1:150)
{
    trainData=iris[setdiff(1:150,i),]
    testData=iris[i,]
    orgfit<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                           data=trainData,control=rpart.control(minsplit=40))
    testData$orgPred<-predict(orgfit,testData,type="class")
    orgErr<-c(orgErr,testData[1,]$Species!=testData[1,]$orgPred)
    newfit<-rpart(Species~.,data=trainData,
                  control=rpart.control(minsplit=40))
    testData$newPred<-predict(newfit,testData,type="class")
    newErr<-c(newErr,testData[1,]$Species!=testData[1,]$newPred)
}
orgErrRate<-sum(orgErr)/150
orgErrRate
## [1] 0.6666667
newErrRate<-sum(newErr)/150
newErrRate
## [1] 0.2933333

# 设置外圆半径
r = 3
x = ((-r * 5):(r * 5))/5
y = sqrt(r^2 - x^2)
vdata = data.frame(x, y = y, type = T)
vdata <- rbind(vdata, data.frame(x, y = -y, type = T))
# 设置内圆半径
r = 1.5
x = ((-r * 10):(r * 10))/10
y = sqrt(r^2 - x^2)
vdata <- rbind(vdata, data.frame(x, y = y, type = F))
vdata <- rbind(vdata, data.frame(x, y = -y, type = F))
colv = rep("red", nrow(vdata))
colv[vdata$type] = "darkgreen"
plot(vdata$x, vdata$y, pch = vdata$type + 3, col = colv, lwd = 2)


head(vdata)
##      x     y type
## 1 -3.0 0.000 TRUE
## 2 -2.8 1.077 TRUE
## 3 -2.6 1.497 TRUE
## 4 -2.4 1.800 TRUE
## 5 -2.2 2.040 TRUE
## 6 -2.0 2.236 TRUE


logitM <- glm(type ~ ., data = vdata, family = binomial(link = "logit"))
vdata$pred = predict.glm(logitM, vdata, type = "response")
summary(vdata$pred)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.5     0.5     0.5     0.5     0.5     0.5

# 设置最高阶数
n = 3
# 建立高阶特征
for (i in 1:2) {
    for (j in 2:n) {
        vdata[, ncol(vdata) + 1] = vdata[, i]^j
    }
}
# 使用新特征建立逻辑回归模型
logitM <- glm(type ~ x + y + V5 + V6 + V7 + V8, data = vdata, family = 
            binomial(link = "logit"), control = glm.control(maxit = 50))
vdata$newPred = predict.glm(logitM, vdata, type = "response")
summary(vdata$newPred)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.5     0.5     1.0     1.0
table(vdata$type, round(vdata$newPred, 2))
##        
##          0  1
##   FALSE 62  0
##   TRUE   0 62


step(logitM)
## Start:  AIC=14
## type ~ x + y + V5 + V6 + V7 + V8
## 
##        Df Deviance   AIC
## - x     1      0.0  12.0
## - y     1      0.0  12.0
## - V6    1      0.0  12.0
## - V8    1      0.0  12.0
## <none>         0.0  14.0
## - V5    1     86.3  98.3
## - V7    1    131.6 143.6
## 
##  省略...
## 
## Step:  AIC=6
## type ~ V5 + V7
## 
##        Df Deviance   AIC
## <none>         0.0   6.0
## - V5    1     86.3  90.3
## - V7    1    131.6 135.6
## 
## Call:  glm(formula = type ~ V5 + V7, family = binomial(link = "logit"), 
##     data = vdata, control = glm.control(maxit = 50))
## 
## Coefficients:
## (Intercept)           V5           V7  
##      -45.94         8.17         8.17  
## 
## Degrees of Freedom: 123 Total (i.e. Null);  121 Residual
## Null Deviance:       172 
## Residual Deviance: 2.65e-10  AIC: 6


plot(vdata$V5,vdata$V7,col=colv,pch=vdata$type+3,lwd=2)

