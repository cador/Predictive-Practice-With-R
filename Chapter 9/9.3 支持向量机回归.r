#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
n=nrow(vdata)
x=as.matrix(vdata[,1:3])
y=as.matrix(vdata[,4])
y=rbind(0,y)
I=t(t(rep(1,n)))
#设置参数sigma
sigma=1
omiga=matrix(rep(0,n*n),ncol=n)
for(i in 1:n)
{
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
    omiga[i,]=exp(-rowSums(deltaX)/(sigma^2))
}
#设置平衡参数gama
gama=10
#构建矩阵A
A=(omiga+(1/gama)*diag(n))
A=cbind(I,A)
A=rbind(c(0,t(I)),A)

#求b和alpha参数
b_alpha=solve(A)%*%y
b=b_alpha[1]
alpha=b_alpha[-1]
#基于vdata进行预测
ypred=NULL
for(i in 1:n)
{
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=3))^2
    ypred=c(ypred,drop(exp(-rowSums(deltaX)/(sigma^2))%*%t(t(alpha)))+b)
}
#查看数据前几行
head(data.frame(y=vdata$y,ypred))
##     y     ypred
## 1 0.2 0.2485166
## 2 0.2 0.2029999
## 3 0.2 0.1689355
## 4 0.2 0.1588311
## 5 0.2 0.2458173
## 6 0.4 0.3122185
#误差平方和
sum((vdata[,4]-ypred)^2)
## [1] 2.867035

