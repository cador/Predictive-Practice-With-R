# 对基础数据进行转换
vdata = iris[, 1:4]
colnames(vdata) = c("x1", "x2", "x3", "y")
x = as.matrix(vdata[, 1:3])
y = as.matrix(vdata[, 4])
# 设置参数
tau = 0.35
n = dim(x)[1]
m = dim(x)[2]
# 设置是否需要截距
hasT=FALSE
# 整理用于单纯形法的输入参量
# 如果考虑截距，则m=m+1,x=cbind(1,x)
if(hasT)
{
m=m+1
x=cbind(1,x)
}
a = c(rep(0, 2 * m), rep(tau, n), rep(1 - tau, n))
A3 = cbind(x, -x, diag(n), -diag(n))
b3 = y
b0 = simplex(a, A3 = A3, b3 = b3)
xcoef = b0$soln[1:m] - b0$soln[(m + 1):(2 * m)]
# 求解的回归系数
xcoef
##      x1      x2      x3 
## -0.1854  0.1250  0.4808

# 对基础数据进行转换
vdata = iris[, 1:4]
colnames(vdata) = c("x1", "x2", "x3", "y")
x = as.matrix(vdata[, 1:3])
y = as.matrix(vdata[, 4])
# 设置参数
tau = 0.35
# ~后面直接加0，表示去掉截距
rq.obj = rq(y ~ 0 + x1 + x2 + x3, data = vdata, tau = tau, method = "br")
rq.obj
## Call:
## rq(formula = y ~ 0 + x1 + x2 + x3, tau = tau, data = vdata, method = "br")
## 
## Coefficients:
##      x1      x2      x3 
## -0.1854  0.1250  0.4808 
## 
## Degrees of freedom: 150 total; 147 residual

library(quantreg)
rq.obj=rq(Petal.Width~Petal.Length,data=iris,tau=2,method="br")
plot(iris[,3:4],pch=20,col='blue',cex=1.5)
cols=rainbow(11)
for(i in 1:11)
{
    if(i==1)
    {
        j=1
    }else if(i==11)
    {
        j=dim(rq.obj$sol)[2]
    }else
    {
        j=i*15
    }
    tmp=rq.obj$sol[,j]
    tcoef=tmp[4]
    xcoef=tmp[5]
    yfit=tcoef+xcoef*iris[,3]
    lines(iris[,3],yfit,col=cols[i])
}
