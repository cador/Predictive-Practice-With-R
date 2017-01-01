#构建R语言函数kalman，对Kalman滤波算法进行实现
#A:状态转移矩阵，默认初始化为diag(ncol(Z))
#H:观测协方差矩阵，默认初始化为diag(ncol(Z))
#Q:系统噪声协方差矩阵，默认初始化为diag(ncol(Z))
#R:观测噪声协方差矩阵，默认初始化为diag(ncol(Z))
#X0:状态量初始值，默认初始化为diag(ncol(Z))
#P0:误差协方差矩阵，默认初始化为diag(ncol(Z))
#Z:观测量
kalman <- function(Z,A=NULL,H=NULL,Q=NULL,R=NULL,X0=NULL,P0=NULL)
{
    dmt <- diag(ncol(Z))
    if(is.null(A))A <- dmt
    if(is.null(H))H <- dmt
    if(is.null(Q))Q <- dmt
    if(is.null(R))R <- dmt
    if(is.null(X0))X0 <- dmt
    if(is.null(P0))P0 <- dmt
    X <- list(X0)
    P <- list(P0)
    N <- nrow(Z)
    I <- diag(dim(A)[1])
    for (i in 1:N) {
        #均方误差的一步预测方程
        Pp <- A %*% P[[i]] %*% t(A) + Q 
        #滤波增益方程（权重）
        K <- Pp %*% t(H) %*% solve(H %*% Pp %*% t(H) + R)
        #状态的一步预测方程
        Xp <- A %*% X[[i]]
        #滤波估计方程（k时刻的最优值）
        X <- c(X, list(Xp + K %*% (diag(Z[i,]) - H %*% Xp)))
        #均方误差更新矩阵（k时刻的最优均方误差）
        P <- c(P, list((I - K %*% H) %*% Pp))
    }
    return(X)
}

Z=Canada[1:84,]
X=kalman(Z)
outMatrix=matrix(unlist(lapply(X,diag)),ncol=n,byrow=T)[2:85,]
par(mfrow=c(2,2))
for(i in 1:n)
{
    a=outMatrix[,i]
    b=Z[,i]
    rag=range(c(a,b))+c(-1,1)
    plot(b,type='l',col='blue')
    lines(a,lty=3,col='red')
}
par(mfrow=c(1,1))

library(FKF)
vdata=read.csv("f:\\vdata001.csv",header=T)
vdata=vdata[,2]
H=matrix(1)
ans <- fkf(a0 = c(0), P0 = matrix(1e6), dt = matrix(0), 
           ct = matrix(0), Tt = matrix(1),
           Zt = matrix(1), HHt = H%*%t(H), GGt =matrix(1), yt = t(vdata))
plot(vdata,type='l')
lines(ans$at[1,][2:(length(vdata)+1)],col='red',lty=3)

