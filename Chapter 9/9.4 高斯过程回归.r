#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
#构建矩阵X和向量y
x=as.matrix(vdata[,1:3])
y=vdata$y
#初始化参数
n=nrow(vdata)
epsilon=1e-5
theta1=1
theta2=1
theta3=1
learnRate=0.005

for(iter in 1:1000)
{
    bigC=matrix(rep(0,n*n),ncol=n)
    for(i in 1:n)
    {
        xi=x[i,]
        deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
        bigC[i,]=exp(2*theta2)*exp(-rowSums(deltaX)/(2*exp(2*theta1)))
    }
    bigC=bigC+diag(rep(exp(2*theta3),n))
    #更新theta1
    delta1=matrix(rep(0,n*n),ncol=n)
    for(i in 1:n)
    {
        xi=x[i,]
        deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
        rsobj=rowSums(deltaX)
        delta1[i,]=exp(2*theta2)*exp(2*theta2)*exp(-rsobj/(2*exp(2*theta1)))*
                   rsobj/(2*exp(2*theta1))
    }
    delta1=0.5*(sum(diag(solve(bigC)%*%delta1))-drop(y%*%(solve(bigC)%*%
           delta1%*%solve(bigC))%*%t(t(y))))
    theta1=theta1-learnRate*delta1
    #更新theta2
    delta2=matrix(rep(0,n*n),ncol=n)
    for(i in 1:n)
    {
        xi=x[i,]
        deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
        delta2[i,]=2*exp(2*theta2)*exp(2*theta2)*
                   exp(-rowSums(deltaX)/(2*exp(2*theta1)))
    }
    delta2=0.5*(sum(diag(solve(bigC)%*%delta2))-drop(y%*%(solve(bigC)%*%
           delta2%*%solve(bigC))%*%t(t(y))))
    theta2=theta2-learnRate*delta2
    #更新theta3
    delta3=diag(rep(2*exp(2*theta3),n))
    delta3=0.5*(sum(diag(solve(bigC)%*%delta3))-drop(y%*%(solve(bigC)%*%
           delta3%*%solve(bigC))%*%t(t(y))))
    theta3=theta3-learnRate*delta3
    print(paste(iter,"---delta1:",delta1,"delta2:",delta2,"delta3:",delta3))
    #当超参数的变化量绝对值的最大值小于给定精度时，退出循环
    if(max(abs(c(delta1,delta2,delta3)))<epsilon)break
}
## [1] "1 ---delta1: -19.569983428 delta2: 31.43436799 delta3: 143.1047867"
## [1] "2 ---delta1: -20.323814675 delta2: 24.08922039 delta3: 139.6311438"
## [1] "3 ---delta1: -20.027273309 delta2: 18.11727918 delta3: 130.4409508"
## [1]......
## [1] "33 ---delta1: -0.097202278 delta2: -0.13839942 delta3: -0.0026393611"
## [1] "34 ---delta1: -0.092042118 delta2: -0.13110796 delta3: -0.0024994293"
## [1]......
## [1] "203 ---delta1: -7.1637e-06 delta2: -1.0245e-05 delta3: -1.947e-07"
## [1] "204 ---delta1: -6.7778e-06 delta2: -9.7202e-06 delta3: -1.882e-07"
#算法迭代次数
iter
## [1] 204
#求得的三个超参数分别为
c(theta1,theta2,theta3)
## [1]  1.509798  0.526808 -1.713836


#进行预测并计算残差平方和
bigC=matrix(rep(0,n*n),ncol=n)
for(i in 1:n)
{
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
    bigC[i,]=exp(2*theta2)*exp(-rowSums(deltaX)/(2*exp(2*theta1)))
}
bigC=bigC+diag(rep(exp(2*theta3),n))
alpha=solve(bigC)%*%y
ypred=NULL
ysigma=NULL
for(i in 1:n)
{
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
    t0=exp(2*theta2)*exp(-rowSums(deltaX)/(2*exp(2*theta1)))
    ypred=c(ypred,drop(t0%*%alpha))
    ysigma=c(ysigma,sqrt(exp(2*theta2)-drop(t0%*%solve(bigC)%*%t(t(t0)))))
}
#最终得到的残差平方和为
print(sum((y-ypred)^2))
## [1] 4.535301
#查看前6行记录
head(data.frame(y,ypred,ysigma))
##     y     ypred     ysigma
## 1 0.2 0.2108910 0.03206466
## 2 0.2 0.1873777 0.04351884
## 3 0.2 0.1687008 0.03558759
## 4 0.2 0.2503346 0.03956528
## 5 0.2 0.2217851 0.03475589
## 6 0.4 0.3791690 0.04448210

library(kernlab)
#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
#构建矩阵X和向量y
x=as.matrix(vdata[,1:3])
y=vdata$y
#建立高斯过程回归模型
gausspr.obj<-gausspr(x, y, scaled = TRUE, type= "regression", kernel="rbfdot",var=1,tol=1e-5,cross=5,)
## Using automatic sigma estimation (sigest) for RBF or laplace kernel
gausspr.obj
## Gaussian Processes object of class "gausspr" 
## Problem type: regression 
## 
## Gaussian Radial Basis kernel function. 
##  Hyperparameter : sigma =  1.138317173214 
## 
## Number of training instances learned : 150 
## Train error : 0.05859663 
## Cross validation error : 0.07162863
#使用高斯过程回归模型进行预测
ypred=predict(gausspr.obj,x)
#计算残差平方和
sum((y-ypred)^2)
## [1] 5.106751

