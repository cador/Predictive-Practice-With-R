# 绘制岭迹曲线 X:自变量的数据矩阵 Y:响应变量向量或矩阵 kMax:岭参数的最大值
# qnum:根据0~kMax分成qnum等分 intT:是否计算截距
plotRidgeCurve <- function(X, Y, kMax = 1, qnum = 10, intT = TRUE) 
{
    if (intT) X <- cbind(t0 = 1, X)
    kvals = c(0, (1:qnum) * (kMax/qnum))
    # 保存回归系数与k的结果
    res <- NULL
    for (k in kvals) {
        tmp = solve(t(X) %*% X + k * diag(ncol(X))) %*% t(X) %*% y
        tmp = rbind(tmp, k = k)
        res <- cbind(res, tmp)
    }
    k <- res[nrow(res), ]
    startRow = ifelse(intT, 2, 1)
    plot(k, res[startRow, ], type = "l", ylim = range(res[startRow:(nrow(res) -1),]) 
+ c(-1, 1), lwd = 2, ylab = "β(k)")
    text(0, res[startRow, 1], "β1", col = "blue", lwd = 2)
    if (nrow(res) > 2) {
        for (i in (startRow + 1):(nrow(res) - 1)) lines(k, res[i, ], lwd = 2)
        text(0, res[i, 1], paste("β", i - startRow + 1, sep = ""), 
col = "blue", lwd = 2)
    }
    abline(h = 0, lty = 2)
    return(res)
}
# 从文件读取M数据集
data = read.csv("f:\\mm.csv")
colnames(data) = c("y", "x1", "x2")
X = as.matrix(data[, 2:3])
y = data$y
plotRidgeCurve(X, Y = y, qnum = 40)
##      [,1]   [,2]    [,3]   [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
## t0 11.292 10.995 10.5381 10.097 9.690 9.320 8.983 8.676 8.396 8.139 7.902
## x1 11.307  6.475  5.1886  4.630 4.337 4.168 4.065 4.000 3.959 3.934 3.919
## x2 -6.591 -1.743 -0.2593  0.509 1.001 1.355 1.626 1.845 2.027 2.182 2.316
## k   0.000  0.025  0.0500  0.075 0.100 0.125 0.150 0.175 0.200 0.225 0.250
##  省略...
##    [,36] [,37] [,38] [,39] [,40] [,41]
## t0 5.089 5.034 4.981 4.931 4.882 4.835
## x1 4.050 4.056 4.061 4.066 4.070 4.075
## x2 3.567 3.587 3.606 3.625 3.642 3.659
## k  0.875 0.900 0.925 0.950 0.975 1.000

# 根据GCV方法，获得最佳岭参数k 
# X:自变量的数据矩阵 
# Y:响应变量向量或矩阵
# kMax:岭参数的最大值 
# qnum:根据0~kMax分成qnum等分 
# intT:是否计算截距
getBestK <- function(X, Y, kMax = 1, qnum = 10, intT = TRUE) {
    if (intT) 
        X <- cbind(t0 = 1, X)
    kvals = c(0, (1:qnum) * (kMax/qnum))
    n = nrow(X)
    glist <- NULL
    for (k in kvals) {
        mk = X %*% solve(t(X) %*% X + k * diag(ncol(X))) %*% t(X)
        yk = mk %*% Y
        Xs <- svd(X)
        d <- Xs$d
        dx <- length(d)
        div <- d^2 + rep(k, rep(dx, 1))
        GCV <- sum((Y - yk)^2)/(n - sum(matrix(d^2/div, dx)))^2
        glist <- c(glist, GCV)
    }
return(list(k = kvals[which.min(glist)], gval = min(glist), 
glist = glist))
}
library(MASS)
x = as.matrix(iris[, 1:3])
y = iris[, 4]
m0 = getBestK(x, t(t(y)), kMax = 1, qnum = 2000)
m0$k
## [1] 0.592
m0$gval
## [1] 0.0002516

library(MASS)
lr.obj <- lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, 
    data = iris, lambda = 0.592)
# 带截距的回归系数
lr.obj
##              Sepal.Length  Sepal.Width Petal.Length 
##      -0.2867      -0.1753       0.1986       0.5065
# 无截距的回归系数
lr.obj$coef
## Sepal.Length  Sepal.Width Petal.Length 
##     -0.14468      0.08627      0.89105
