acf(airmiles,type='correlation',lag.max=10)
pacf(airmiles,lag.max=10)
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris, 
   main="Simple Scatterplot Matrix")
scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species, 
       data=iris)
scatter3d(iris$Sepal.Length, iris$Petal.Length, iris$Petal.Width)
library(corrgram)
#1、设置排序处理
corrgram(mtcars,order=TRUE)
#2、设置上下三角面板形状
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie)
#3、只显示下三角部分
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,upper.panel=NULL)
#4、调整面板颜色
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,
         col.regions=colorRampPalette(c("darkgoldenrod4","burlywood1","white",
         "darkkhaki","darkgreen")))
library(corrplot)
#1、使用不同的method绘制相关矩阵图
methods<-c("circle","square","ellipse","pie","shade","color")
par(mfrow=c(2,3))
t0=mapply(function(x){corrplot(M, method=x,order="AOE")},methods)
par(mfrow=c(1,1))
#2、设置method=color绘制热力矩阵图
corrplot(cor(mtcars), method="color", order = "AOE",tl.col="black",tl.srt=45,
         addCoef.col="black",col=colorRampPalette(c("#7F0000","red","#FF7F00",
         "yellow","white", "cyan", "#007FFF", "blue","#00007F"))(20))
#3、绘制上下三角及不同色彩的相关矩阵图
library(RColorBrewer)
par(mfrow=c(2,2))
corrplot(cor(mtcars),type="lower")
corrplot(cor(mtcars),type="lower",order="hclust",
            col=brewer.pal(n=8,name="RdYlBu"))
corrplot(cor(mtcars),type="upper",order="AOE",
            col=c("black","white"),bg="lightblue")
corrplot(cor(mtcars),type="upper",order="FPC",
            col=brewer.pal(n=8, name="PuOr"))
par(mfrow=c(1,1))

d<-sqrt(1-cor(mtcars)^2)
hc<-hclust(as.dist(d))
plot(hc)
rect.hclust(hc,k=3)

library(pvclust)
cluster.bootstrap <- pvclust(mtcars, nboot=1000, method.dist="correlation")
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)

#1、提取iris的前4个数值列，并进行标准化处理
data0=scale(iris[1:4])
#2、计算这4个变量的协方差，由于经过标准化处理，这样得到的也是相关系数
M=cov(data0)
#3、将M进行分块，1:2两个变量一组，3:4是另外一组，并进行两两组合
X11=M[1:2,1:2]
X12=M[1:2,3:4]
X21=M[3:4,1:2]
X22=M[3:4,3:4]
#4、按公式求解矩阵A和B
A=solve(X11)%*%X12%*%solve(X22)%*%X21
B=solve(X22)%*%X21%*%solve(X11)%*%X12
#5、使用eigen函数求解典型相关系数如下
eV=sqrt(eigen(A)$values)
eV

#6、进行验证
#...比较A与XΛX^(-1)是否相等
round(A-eigen(A)$vectors%*%diag(eigen(A)$values)%*%solve(eigen(A)$vectors),3)
##              Sepal.Length Sepal.Width
## Sepal.Length            0           0
## Sepal.Width             0           0
#...比较B与YΛY^(-1)是否相等
round(B-eigen(B)$vectors%*%diag(eigen(B)$values)%*%solve(eigen(B)$vectors),3)

#...求解A对应的特征向量并计算典型向量C1
C1=data0[,1:2]%*%eigen(A)$vectors
#...验证C1对应各变量的标准差是否为1，同时查看均差
apply(C1,2,sd)
## [1] 1.041196 0.951045
apply(C1,2,mean)
## [1] -4.880321e-16 -2.759430e-17
#...由于均值为0，标准差不为1，这里对特征向量进行伸缩变换
eA=eigen(A)$vectors%*%diag(1/apply(C1,2,sd))
#...再次验证方差和均值
C1=data0[,1:2]%*%eA
apply(C1,2,sd)
## [1] 1 1
apply(C1,2,mean)
## [1] -4.667693e-16 -2.745503e-17
#...可见，特征向量已经满足要求，同理对B可得
C2=data0[,3:4]%*%eigen(B)$vectors
apply(C2,2,sd)
## [1] 0.6291236 0.2003530
apply(C2,2,mean)
## [1] -1.403572e-17 -9.859870e-18
eB=eigen(B)$vectors%*%diag(1/apply(C2,2,sd))
C2=data0[,3:4]%*%eB
apply(C2,2,sd)
## [1] 1 1
apply(C2,2,mean)

round(cor(cbind(C1,C2)),3)

x<-as.matrix(iris[,1:2])
y<-as.matrix(iris[,3:4])
cancor(x,y)
