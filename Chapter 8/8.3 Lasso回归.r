y=iris$Petal.Width
x=as.matrix(iris[,1:3])
t0=apply(x,2,mean)
x=scale(x,t0,F)
t1=apply(x,2,function(x)sqrt(sum(x^2)))
x=scale(x,F,t1)
m=dim(x)[2]
colnames(x)=paste("x",1:m,sep="")
y=y-mean(y)

#非活动变量与残差的相关系数
C=NULL
Sign<-NULL
#非活动变量下标集合
im <- inactive <- seq(m)
#活动变量下标集合
active <- NULL
eps=.Machine$double.eps
k=0
max.steps=8*m
#初始化回归系数矩阵
beta <- matrix(0, max.steps + 1, m)
colnames(beta)=paste("x",1:m,sep="")
#计算y与x的相关性
Cvec <- drop(t(y) %*% x)
#被忽略的变量下标集合
ignores=NULL

library(Matrix)
while(k<max.steps & length(active)<m)
{
    C<-Cvec[inactive]
    Cmax<-max(abs(C))
    if (Cmax < eps * 100) {
        print("最大的相关系数为0，退出循环\n")
        break
    }
    k=k+1
    new <- abs(C) >= Cmax - eps
    C <- C[!new]
    new <- inactive[new]
    for (inew in new) 
    {
        if(rankMatrix(as.matrix(x[,c(inew,active)]))==length(active))
        {
            ignores <- c(ignores, inew)
        }else{
            active <- c(active, inew)
            Sign <- c(Sign, sign(Cvec[inew]))
        }
    }
    active.len=length(active)
    inactive <- im[-c(active, ignores)]
    xa=matrix(rep(Sign,dim(x)[1]),ncol=active.len,byrow=T)*x[,active]
    oneA=t(t(rep(1,active.len)))
    A=drop((t(oneA)%*%solve(t(xa)%*%xa)%*%oneA)^-0.5)
    w=drop(A*solve(t(xa)%*%xa)%*%oneA)
    if(active.len>=m){
        gamhat <- Cmax/A
    }else{
        a=drop(t(x[,inactive])%*%xa%*%t(t(w)))
        gam <- c((Cmax - C)/(A - a), (Cmax + C)/(A + a))
        gamhat <- min(gam[gam > eps], Cmax/A)
    }
    b1 <- beta[k, active]
    z1 <- -b1/(w*Sign)
    zmin <- min(z1[z1 > eps], gamhat)
    gamhat<-ifelse(zmin < gamhat,zmin,gamhat)
    beta[k + 1, active] <- beta[k, active] + gamhat * w*Sign
    Cvec <- Cvec - gamhat * t(xa%*%t(t(w)))%*%x
}
beta=beta[1:(k+1),]

beta
##          x1     x2     x3
## [1,]  0.000 0.0000  0.000
## [2,]  0.000 0.0000  8.657
## [3,]  0.000 0.2763  8.933
## [4,] -2.095 1.1855 11.293

lar.obj = lars(x, y, type = "lasso")
lar.obj$beta
##       x1     x2     x3
## 0  0.000 0.0000  0.000
## 1  0.000 0.0000  8.657
## 2  0.000 0.2763  8.933
## 3 -2.095 1.1855 11.293
## attr(,"scaled:scale")
## [1] 1 1 1

plot(lar.obj)
