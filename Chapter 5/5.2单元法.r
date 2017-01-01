cor.test(iris$Sepal.Length,iris$Petal.Length)
## 
##  Pearson's product-moment correlation
## 
## data:  iris$Sepal.Length and iris$Petal.Length
## t = 21.646, df = 148, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.8270363 0.9055080
## sample estimates:
##       cor 
## 0.8717538

x=runif(100,-1,1)
y=x^2
plot(x,y)

cor.test(x,y)
## 
##  Pearson's product-moment correlation
## 
## data:  x and y
## t = 0.65787, df = 98, p-value = 0.5122
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1318261  0.2593492
## sample estimates:
##        cor 
## 0.06630892

library(energy)
x=runif(100,-1,1)
y=x^2
dcor(x,y)
## [1] 0.5044376

#单因素方差分析
#xdata：data.frame，至少包含因素列和指标列
#factorNo:a numeric，因素列下标
#NumNo:a numeric，指标列下标
OneWayAnova<-function(xdata,factorNo,NumNo)
{
    pnormTest<-NULL
    homoTest<-NULL
    #将因素水平对应字段转成因子类型
    xdata[,factorNo]<-as.factor(xdata[,factorNo])
    #正态性检验
    Lvls<-levels(xdata[,factorNo])
    for(i in 1:length(Lvls))
    {
        tmp<-shapiro.test(xdata[xdata[,factorNo]==Lvls[i],NumNo])
        res<-NULL
        if(tmp$p.value<0.01)
        {
            pnormTest<-c(pnormTest,paste("水平 - ",Lvls[i]," , 正态检测的P值 : "
                         ,tmp$p.value, "显著性差异，不服从正态分布。",sep=""))
        }
        else
        {
            pnormTest<-c(pnormTest,paste("水平 - ",Lvls[i]," , 正态检测的P值 : "
                         ,tmp$p.value,"差异不显著，服从正态分布。",sep=""))
        }
    }

    #方差齐性检验
    tmp<-bartlett.test(xdata[,NumNo],xdata[,factorNo])
    homoTest<-paste("方差齐性检验 - P值 : ",tmp$p.value,sep="")
    if(tmp$p.value<0.01)
    {
        homoTest<-paste(homoTest,paste("显著性差异，方差不是齐性的。",sep=""))
    }
    else
    {
        homoTest<-paste(homoTest,paste("差异不显著，方差是齐性的。",sep=""))
    }
    
    #单因素方差分析
    E<-aov(xdata[,NumNo]~xdata[,factorNo],data=xdata)
    return(list(pnormTest,homoTest,summary(E)))
}

OneWayAnova(iris,5,2)
## [[1]]
## [1] "水平-setosa,正态检测的P值 : 0.271526393904差异不显著，服从正态分布。"    
## [2] "水平-versicolor,正态检测的P值 : 0.337995108260差异不显著，服从正态分布。"
## [3] "水平-virginica,正态检测的P值 : 0.18089604036差异不显著，服从正态分布。"  
## 
## [[2]]
## [1] "方差齐性检验 - P值 : 0.351502800415803 差异不显著，方差是齐性的。"
## 
## [[3]]
##                    Df Sum Sq Mean Sq F value Pr(>F)    
## xdata[, factorNo]   2  11.35   5.672   49.16 <2e-16 ***
## Residuals         147  16.96   0.115                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

library(discretization)
disc=chiM(iris,alpha=0.05)
str(disc)
## List of 2
##  $ cutp     :List of 4
##   ..$ : num [1:3] 5.45 5.75 7.05
##   ..$ : num [1:2] 2.95 3.35
##   ..$ : num [1:3] 2.45 4.75 5.15
##   ..$ : num [1:2] 0.8 1.75
##  $ Disc.data:'data.frame':   150 obs. of  5 variables:
##   ..$ Sepal.Length: int [1:150] 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ Sepal.Width : int [1:150] 3 2 2 2 3 3 3 3 1 2 ...
##   ..$ Petal.Length: int [1:150] 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ Petal.Width : int [1:150] 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 
##                     1 1 1 1 1 1 1 1 1 1 ...
summary(disc$Disc.data)
##   Sepal.Length    Sepal.Width     Petal.Length   Petal.Width   
##  Min.   :1.000   Min.   :1.000   Min.   :1.00   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.00   1st Qu.:1.000  
##  Median :3.000   Median :2.000   Median :2.00   Median :2.000  
##  Mean   :2.247   Mean   :1.867   Mean   :2.26   Mean   :1.973  
##  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:3.00   3rd Qu.:3.000  
##  Max.   :4.000   Max.   :3.000   Max.   :4.00   Max.   :3.000  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
head(disc$Disc.data)
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1            1           3            1           1  setosa
## 2            1           2            1           1  setosa
## 3            1           2            1           1  setosa
## 4            1           2            1           1  setosa
## 5            1           3            1           1  setosa
## 6            1           3            1           1  setosa

#该函数用于计算U作为信源信号、V作为接收信号时的信息增益
#u：信源信号，分类向量或因子
#v：接收信号，分类向量或因子
gains <- function(u,v)
{
    #0、预处理，将u、v强制转换成因子类型
    u <- as.factor(u)
    v <- as.factor(v)
    #1、计算u的概率向量
    u_pv <- table(u)/length(u)
    #2、计算Ent(U)
    ent_u <- (-1)*sum(u_pv*log2(u_pv))
    #3、计算v的概率向量
    v_pv <- table(v)/length(u)
    #4、计算v到u的条件概率矩阵
    v0 <- as.matrix(table(v,u))
    cpm <- v0/apply(v0,1,sum)
    #5、计算Ent(U|V)
    ent_uv <- sum(v_pv*apply(cpm,1,function(x){
                 x <- x[x>0]
                 return(-sum(x*log2(x)))
              }))
    #6、计算信息增益
    gains_uv <- (ent_u - ent_uv)
    return(gains_uv)
}
library(discretization)
disc=chiM(iris,alpha=0.05)
u=disc$Disc.data$Species
v=disc$Disc.data$Sepal.Length
out=gains(u,v)
print(out)
## [1] 0.7285454

s2=disc$Disc.data$Sepal.Width
p1=disc$Disc.data$Petal.Length
p2=disc$Disc.data$Petal.Width
gains_us2=gains(u,s2)
gains_up1=gains(u,p1)
gains_up2=gains(u,p2)
print(gains_us2)
## [1] 0.3855963
print(gains_up1)
## [1] 1.418003
print(gains_up2)
## [1] 1.378403

#该函数用于计算U作为信源信号、V作为接收信号时的信息增益率
#u：信源信号，分类向量或因子
#v：接收信号，分类向量或因子
gainsR <- function(u,v)
{
    #0、预处理，将u、v强制转换成因子类型
    u <- as.factor(u)
    v <- as.factor(v)
    #1、计算u的概率向量
    u_pv <- table(u)/length(u)
    #2、计算Ent(U)
    ent_u <- (-1)*sum(u_pv*log2(u_pv))
    #3、计算v的概率向量
    v_pv <- table(v)/length(u)
    #4、计算v到u的条件概率矩阵
    v0 <- as.matrix(table(v,u))
    cpm <- v0/apply(v0,1,sum)
    #5、计算Ent(U|V)
    ent_uv <- sum(v_pv*apply(cpm,1,function(x){
                 x <- x[x>0]
                 return(-sum(x*log2(x)))
              }))
    #6、计算Ent(V)
    ent_v <- (-1)*sum(v_pv*log2(v_pv))
    #7、计算信息增益率
    gainsR_uv <- (ent_u - ent_uv)/ent_v
    return(gainsR_uv)
}
u=disc$Disc.data$Species
s1=disc$Disc.data$Sepal.Length
s2=disc$Disc.data$Sepal.Width
p1=disc$Disc.data$Petal.Length
p2=disc$Disc.data$Petal.Width
ent_us1=gainsR(u,s1)
ent_us2=gainsR(u,s2)
ent_up1=gainsR(u,p1)
ent_up2=gainsR(u,p2)
print(ent_us1)
## [1] 0.4184032
print(ent_us2)
## [1] 0.2472972
print(ent_up1)
## [1] 0.733996
print(ent_up2)
## [1] 0.8713692

library(discretization)
disc=chiM(iris,alpha=0.05)
library(FSelector)
#计算信息增益
wt1 <- information.gain(Species~.,data=disc$Disc.data)
print(wt1)
##              attr_importance
## Sepal.Length       0.4608686
## Sepal.Width        0.2672750
## Petal.Length       0.9402853
## Petal.Width        0.9554360
#计算信息增益率
wt2 <- gain.ratio(Species~.,data=disc$Disc.data)
print(wt2)
##              attr_importance
## Sepal.Length       0.4679736
## Sepal.Width        0.2472972
## Petal.Length       0.8584937
## Petal.Width        0.8713692

library(discretization)
disc=chiM(iris,alpha=0.05)
chisq.test(disc$Disc.data$Sepal.Width,disc$Disc.data$Species)
## 
##  Pearson's Chi-squared test
## 
## data:  disc$Disc.data$Sepal.Width and disc$Disc.data$Species
## X-squared = 72.683, df = 4, p-value = 6.156e-15

library(discretization)
disc=chiM(iris,alpha=0.05)
library(FSelector)
#基于卡方检验计算特征重要性
chi.squared(Species~.,data=disc$Disc.data)
##              attr_importance
## Sepal.Length       0.6398741
## Sepal.Width        0.4922162
## Petal.Length       0.9346311
## Petal.Width        0.9432359
chi.squared
## function (formula, data) 
## {
##     new_data = get.data.frame.from.formula(formula, data)
##     new_data = discretize.all(formula, new_data)
##     class_data = new_data[[1]]
##     new_data = new_data[-1]
##     results = sapply(new_data, function(w) {
##         cont = table(class_data, w)
##         row_sums = apply(cont, 1, sum)
##         col_sums = apply(cont, 2, sum)
##         all_sum = sum(col_sums)
##         expected_matrix = t(as.matrix(col_sums) %*% t(as.matrix(row_sums)))
##                            /all_sum
##         chis = sum((cont - expected_matrix)^2/expected_matrix)
##         if (chis == 0 || length(col_sums) < 2 || length(row_sums) < 
##             2) {
##             return(0)
##         }
##         else {
##             return(sqrt(chis/(all_sum * min(length(col_sums) - 
##                 1, length(row_sums) - 1))))
##         }
##     })
##     attr_names = dimnames(new_data)[[2]]
##     return(data.frame(attr_importance = results, row.names = attr_names))
## }
## <environment: namespace:FSelector>

library(rpart)
library(maptree)
## Loading required package: cluster
rpart.fit <- rpart(Species~.,data=iris)
draw.tree(rpart.fit)

rpart.fit$variable.importance
##  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
##     88.96940     81.34496     54.09606     36.01309
barplot(rpart.fit$variable.importance)

