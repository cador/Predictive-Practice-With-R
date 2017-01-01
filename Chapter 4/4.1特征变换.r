#加载数据集，这里使用荷兰男孩的身体发育数据boysData
boysData<-read.csv("boysData.csv")
#属性说明：
#age:年龄
#hgt:身高
#wgt:体重
#bmi:体重指数
#hc:头围
#gen:发育程度1
#phb:发育程度2
#tv:发育程度3
#reg:所属区域
print(head(boysData))
##     age  hgt   wgt   bmi   hc  gen  phb tv   reg
## 1 0.035 50.1 3.650 14.54 33.7 <NA> <NA> NA south
## 2 0.038 53.5 3.370 11.77 35.0 <NA> <NA> NA south
## 3 0.057 50.0 3.140 12.56 35.2 <NA> <NA> NA south
## 4 0.060 54.5 4.270 14.37 36.7 <NA> <NA> NA south
## 5 0.062 57.5 5.030 15.21 37.3 <NA> <NA> NA south
## 6 0.068 55.5 4.655 15.11 37.0 <NA> <NA> NA south
summary(boysData)
##       age              hgt              wgt              bmi       
##  Min.   : 0.035   Min.   : 50.00   Min.   :  3.14   Min.   :11.77  
##  1st Qu.: 1.581   1st Qu.: 84.88   1st Qu.: 11.70   1st Qu.:15.90  
##  Median :10.505   Median :147.30   Median : 34.65   Median :17.45  
##  Mean   : 9.159   Mean   :132.15   Mean   : 37.15   Mean   :18.07  
##  3rd Qu.:15.267   3rd Qu.:175.22   3rd Qu.: 59.58   3rd Qu.:19.53  
##  Max.   :21.177   Max.   :198.00   Max.   :117.40   Max.   :31.74  
##                   NA's   :20       NA's   :4        NA's   :21     
##        hc          gen        phb            tv           reg     
##  Min.   :33.70   G1  : 56   P1  : 63   Min.   : 1.00   city : 73  
##  1st Qu.:48.12   G2  : 50   P2  : 40   1st Qu.: 4.00   east :161  
##  Median :53.00   G3  : 22   P3  : 19   Median :12.00   north: 81  
##  Mean   :51.51   G4  : 42   P4  : 32   Mean   :11.89   south:191  
##  3rd Qu.:56.00   G5  : 75   P5  : 50   3rd Qu.:20.00   west :239  
##  Max.   :65.00   NA's:503   P6  : 41   Max.   :25.00   NA's :  3  
##  NA's   :46                 NA's:503   NA's   :522
#这里根据BMI指数，将泛化成体重类型的wtype字段
#通过summary的结果得知bmi字段存在21个缺失值与总量748相比远小于5%，这里将其删除
boysData<-boysData[!is.na(boysData$bmi),]
#设置变换规则
typeUp<-c(18.5,24.99,25,28,32,100)
typeDown<-c(0,18.5,20,25,28,32)
typeName<-c("过轻","正常","适中","过重","肥胖","非常肥胖")
boysData$wtype<-typeName[unlist(mapply(function(x){
    tmp<-intersect(which(typeDown<x),which(typeUp>=x))
    #如果同时满足正常和适中，则默认为适中
    return(tmp[length(tmp)])
},boysData$bmi))]
head(boysData)
##     age  hgt   wgt   bmi   hc  gen  phb tv   reg wtype
## 1 0.035 50.1 3.650 14.54 33.7 <NA> <NA> NA south  过轻
## 2 0.038 53.5 3.370 11.77 35.0 <NA> <NA> NA south  过轻
## 3 0.057 50.0 3.140 12.56 35.2 <NA> <NA> NA south  过轻
## 4 0.060 54.5 4.270 14.37 36.7 <NA> <NA> NA south  过轻
## 5 0.062 57.5 5.030 15.21 37.3 <NA> <NA> NA south  过轻
## 6 0.068 55.5 4.655 15.11 37.0 <NA> <NA> NA south  过轻
barplot(table(boysData$wtype),col=rainbow(9),border='gray')

#该函数用于获取数据x的各种标准化值
#x:用于标准化的实数向量
#isPos:是否是正向指标
stdProc<-function(x,isPos)
{
    #(1)线性标准化
    #---极差标准化
    if(max(x)>min(x)){
        if(isPos){
            yExt=(x-min(x))/(max(x)-min(x))
        }else{
            yExt=(max(x)-x)/(max(x)-min(x))
        }
    }else{
        print("最大值与最小值相等，不能进行极差标准化!")
        yExt=NULL
    }
    
    #---z-score标准化
    sd0<-sd(x)
    mean0<-mean(x)
    if(sd0==0){
        print("由于标准差为0，不能进行z-score标准化")
        yZsc=NULL
    }else{
        yZsc=(x-mean0)/sd0
    }
    #---小数定标标准化
    yPot=x/(10^nchar(max(abs(x))))
    
    #(2)非线性标准化
    #---对数标准化
    if(isPos){
        y=log(x-min(x)+1)
        yLog=(1/max(y))*y
    }else{
        y=log(max(x)-x+1)
        yLog=(1/max(y))*y
    }
    #---倒数标准化
    yInv=min(abs(x[x!=0]))/x
    
    return(list(yExt=yExt,yZsc=yZsc,yPot=yPot,yLog=yLog,yInv=yInv))
}


#按均匀分布生成100个介于10到100之间的实数
tmpV=runif(100,10,100)
#1.使用quantile函数进行等比分箱，此处将数据分成4份
newType=c("A1","A2","A3","A4")
q0=quantile(tmpV,probs=seq(0,1,1/4))
v0=rep(newType[1],length(tmpV))
for(i in 2:(length(q0)-1)){
    v0[tmpV>q0[i] & tmpV<=q0[i+1]]=newType[i]
}
#...另外常可通过均值、中位数、最大最小值来平滑数值以生成新的特征
vt0=tmpV[tmpV>=q0[1] & tmpV<=q0[2]]
v_mean=rep(mean(vt0),length(tmpV))
v_median=rep(median(vt0),length(tmpV))
v_max=rep(max(vt0),length(tmpV))
v_min=rep(min(vt0),length(tmpV))
for(i in 2:(length(q0)-1)){
    v_mean[tmpV>q0[i] & tmpV<=q0[i+1]]=mean(tmpV[tmpV>q0[i] & tmpV<=q0[i+1]])
    v_median[tmpV>q0[i] & tmpV<=q0[i+1]]=median(tmpV[tmpV>q0[i] &tmpV<=q0[i+1]])
    v_max[tmpV>q0[i] & tmpV<=q0[i+1]]=max(tmpV[tmpV>q0[i] & tmpV<=q0[i+1]])
    v_min[tmpV>q0[i] & tmpV<=q0[i+1]]=min(tmpV[tmpV>q0[i] & tmpV<=q0[i+1]])
}
#2.使用cut函数进行等宽分箱，此处将数据分成5份
c0=cut(tmpV,breaks=5,labels=c("B1","B2","B3","B4","B5"))
#...另外可通过设置labels为NULL，并通过levles函数查看cut的水平
#...进一步确定各分箱的取值区间
#...可通过均值、中位数、最大最小值来平滑数值以生成新的特征
L0=levels(cut(tmpV,breaks=5))
v2_mean=v2_median=v2_max=v2_min=rep(0,length(tmpV))
for(lvl in L0)
{
    splitval=as.integer(strsplit(strsplit(strsplit(lvl,split='\\(')[[1]][2],
              split='\\]')[[1]],split=',')[[1]])
    subcond=tmpV>splitval[1] & tmpV<=splitval[2]
    subval=tmpV[subcond]
    v2_mean[subcond]=mean(subval)
    v2_median[subcond]=median(subval)
    v2_max[subcond]=max(subval)
    v2_min[subcond]=min(subval)
}

#将iris中的Sepal.Length变量排序，并保存在变量sortedSL中
sortedSL=sort(iris$Sepal.Length)
#循环：按顺序以sortedSL中的每个值作为分割点，重建新特征new，默认设置为0
gainsV<-NULL
splitV<-NULL
for(i in 1:NROW(sortedSL))
{
    splitVal=sortedSL[i]
    iris$new=0
    if(sum(iris$Sepal.Length>splitVal)>0)
    {
        iris[iris$Sepal.Length>splitVal,]$new=1
    }
    gainsV<-c(gainsV,gains(iris$Species,iris$new))
    splitV<-c(splitV,splitVal)
}
#分割点为
finalSplitV<-splitV[which.max(gainsV)]
finalSplitV
## [1] 5.5

