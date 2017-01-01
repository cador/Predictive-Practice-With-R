#1.读取列联表到数据框
vdata=read.csv("f:\\demolaolinhua.csv",header=T,row.names = 1)
#2.加载MASS包，用corresp函数进行对应分析
library(MASS)
cp<-corresp(vdata,nf=2)
cp
## First canonical correlation(s): 0.29614629 0.08462696 
## 
##  Row scores:
##              [,1]        [,2]
## 很好   -0.8598235  0.90764251
## 好     -0.4243279  0.31560800
## 一般   -0.3176986  0.02177613
## 差      1.1427620 -1.80803262
## 很差    4.6633068  4.82861892
## 没回答  4.0033447  1.24147941
## 
##  Column scores:
##                [,1]       [,2]
## 完全自理 -0.5368034  0.2556804
## 部分自理  0.7824802 -2.2683088
## 不能自理  2.4804060  1.2965027
#3.绘制二维图表
rgx<-range(cp$rscore[,1],cp$cscore[,1])+c(-0.5,0.5)
rgy<-range(cp$rscore[,2],cp$cscore[,2])+c(-0.5,0.5)
plot(cp$rscore,pch=20,cex=5,col='red',xlim=rgx,ylim=rgy,xlab="Dimension 1",
     ylab="Dimension 2")
text(cp$rscore-0.2,rownames(cp$rscore))
points(cp$cscore,pch=20,cex=5,col='darkgreen')
text(cp$cscore-0.2,rownames(cp$cscore))
abline(v=0,h=0,lty=3)

