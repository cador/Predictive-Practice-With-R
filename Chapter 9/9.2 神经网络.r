#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3，并增加x0，并初始化为1向量
vdata[,1:3]=scale(vdata[,1:3])
vdata=cbind(x0=1,vdata)
#初始化精度控制参数ε
epsilon=5.5
#初始化学习效率α
alpha=0.005
#精度控制变量d
d=epsilon+1
#用适当小的随机数初始化权向量W
w=runif(4,0,1)
while(d>=epsilon)
{
    d=0
    for(i in 1:nrow(vdata))
    {
        xi=vdata[i,1:4]
        delta=sum(xi*w)-vdata[i,5]
        w=w-alpha*delta*xi
        d=d+delta^2
    }
    print(d)
}
## [1] 88.15584
## [1] 27.61774
## [1] 13.66231
## [1] 10.38355
## [1] 9.452375
## [1] 9.009101
## [1] 8.668056
## [1] 8.359672
## [1] 8.075274
## [1] ......
## [1] 7.172267
## [1] ......
## [1] 6.189793
## [1] 6.116226
## [1] 6.050154
## [1] ......
## [1] 5.6894
## [1] ......
## [1] 5.498515

w
##        x0         x1         x2        x3
## 1 1.21412 -0.1346637 0.08743699 0.8941653
sum((as.matrix(vdata[,1:4])%*%t(w)-vdata[,5])^2)
## [1] 5.450383

lm.obj=lm(y~x1+x2+x3,data=vdata)
lm.obj
## 
## Call:
## lm(formula = y ~ x1 + x2 + x3, data = vdata)
## 
## Coefficients:
## (Intercept)           x1           x2           x3  
##     1.19933     -0.17163      0.09712      0.92516
sum(summary(lm.obj)$residual^2)
## [1] 5.380298

#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3，并增加x0，并初始化为1向量
vdata[,1:3]=scale(vdata[,1:3])
vdata=cbind(x0=1,vdata)
#设定学习效率alpha
alpha=0.01
#评估隐含层神经元个数
m=round(sqrt(0.43*1*4+0.12+2.54*4+0.77+0.35)+0.51,0)
#初始化输入向量的权重矩阵
wInput<-matrix(runif(4*m,-1,1),ncol=4)
#初始化隐含层到输出的权重向量
wHide<-runif(m,-1,1)
epsilon=1e-3
errorList=NULL

#进入迭代
for(iter in 1:1000)
{
    error=0
    for(i in 1:nrow(vdata))
    {
        #正向传播过程
        xInput<-as.matrix(vdata[i,1:4])
        rownames(xInput)=NULL
        yi=vdata[i,5]
        d=wInput%*%t(xInput)
        z=(exp(d)-exp(-d))/(exp(d)+exp(-d))
        o=drop(wHide%*%z)
        e=o-yi
        error=error+e^2
        #若e>epsilon，则进入反向传播过程
        if(abs(e)>epsilon)
        {
            wHide=wHide-alpha*z[,1]*e
            wInput=wInput-tcrossprod((4*exp(2*d)/((exp(2*d)+1)^2))[,1]*
                                      wHide*alpha*e,xInput[1,])
        }
    }
    errorList=c(errorList,error)
    print(paste("iter:",iter," error:",error))
    #当连续两次残差平方和的差小于epsilon时，退出循环
if(length(errorList)>2 && 
rev(errorList)[2]-rev(errorList)[1]<epsilon)break
}
## [1] "iter: 1  error: 66.4131176626784"
## [1] "iter: 2  error: 16.847988698505"
## [1] "iter: 3  error: 13.4037341061902"
## [1] "iter: 4  error: 12.2846423671004"
## [1] "iter: 5  error: 11.3630907332195"
## [1] "iter: 6  error: 10.6133525205955"
## [1] "iter: 7  error: 10.0039276748159"
## [1] ......省略
## [1] "iter: 83  error: 5.5604610326813"
## [1] ...... 省略
## [1] "iter: 188  error: 5.03248616492139"
## [1] ...... 省略
## [1] "iter: 279  error: 4.67760230129623"
## [1] ...... 省略
## [1] "iter: 394  error: 4.48283637403827"
## [1] "iter: 395  error: 4.4818403631067"

#最终的残差平方和为
error
## [1] 4.48184
#求得的隐含层权重及阈值为
wInput
##             [,1]        [,2]        [,3]      [,4]
## [1,]  0.08804399 -1.00892141 -0.70423934 0.3044293
## [2,]  0.52380113 -0.76984368  0.53643432 0.1736050
## [3,]  0.84747633 -0.01276955 -0.09988584 0.2522201
## [4,] -0.31832200  0.01582337 -0.15897845 1.0520356
#求得隐含层到输出层的权重为
wHide
## [1] -0.2245227  0.4500343  1.7862454  0.9213183

#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])

#使用hclust函数，进行聚类分析
lambda=0.2
vdata.hclst<-hclust(dist(vdata[,1:3]))
vdata$type=cutree(vdata.hclst,h=lambda)
plot(vdata.hclst)
rect.hclust(vdata.hclst,h=lambda)

#隐含层神经元个数p为
p=length(unique(vdata$type))
p
## [1] 113
#各类的中心分别为ci~p
centers=aggregate(vdata[,1:3],by=list(vdata$type),FUN=mean)
centers$Group.1=NULL
centers=as.matrix(centers)
#各类的扩展常数为σ(sigma)
alpha=1
dst=as.matrix(dist(as.matrix(centers[,1:3])))
dst[dst==0]=max(dst)
sigmas=alpha*apply(dst,1,min)

#构建隐含层神经元，得到Φ矩阵
phiMatrix=NULL
for(i in 1:nrow(vdata))
{
    xi=as.matrix(vdata[i,1:3])
    tmp=t(matrix(rep(xi,p),ncol=p))
phiMatrix=rbind(phiMatrix,diag(exp(-((tmp-centers)%*%
t(tmp-centers))/(sigmas^2))))
}
phiMatrix=cbind(1,phiMatrix)
colnames(phiMatrix)=NULL
#求解权向量W为
W=(solve(t(phiMatrix)%*%phiMatrix+diag(rep(1e-5,p+1)))%*%
t(phiMatrix))%*%t(t(vdata$y))
W
##                 [,1]
##   [1,]  0.9702855994
##   [2,]  0.0173171927
##   [3,] -0.3430261486
##   ......省略

#计算残差平方和
sum(((phiMatrix%*%W)[,1]-vdata$y)^2)
## [1] 1.255916

#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
#设置隐含层数量n、输入神经元数量r
n=5
r=3
#初始化W1、W2、W3权重矩阵或向量及其改变量
W1=matrix(runif(n*n,-1,1),ncol=n)
W2=matrix(runif(r*n,-1,1),ncol=r)
W3=runif(n,-1,1)
xc=rep(0,n)
xk_1=rep(0,n)
deltaW1=matrix(rep(0,n*n),ncol=n)
deltaW2=matrix(rep(0,r*n),ncol=r)
deltaW3=rep(0,n)
#初始化参数
mc=0.9
eta=0.01
alpha=0.3
epsilon=1e-5
errorList=NULL


#进入迭代并更新权重矩阵或向量
for(iter in 1:5000)
{
    error=0
    for(rowid in 1:nrow(vdata))
    {
        u=as.matrix(vdata[rowid,1:3])
        x=(1/(1+exp(-(W1%*%t(t(xc))+W2%*%t(u)))))[,1]
        xc=alpha*xc+xk_1
        ok=drop(x%*%t(t(W3)))
        e=ok-vdata[rowid,4]
        error=error+e^2
        #若误差e的绝对值大于epsilon则进入反向传播
        if(abs(e)>epsilon)
        {
            deltaW3=(1-mc)*eta*(-e)*x+mc*deltaW3
            deltaW2=tcrossprod((1-mc)*eta*(-e)*W3*(x-x^2),u[1,])+mc*deltaW2
            for(j in 1:n)
            {
                deltaW1[j,]=eta*(-e)*W3[j]*((x-x^2)[j]*xk_1+alpha*deltaW1[j,])
            }
        }
        xk_1=x
        W3=W3+deltaW3
        W2=W2+deltaW2
        W1=W1+deltaW1
    }
    errorList=c(errorList,error)
    print(paste("iter:",iter," error:",error))
    #当残差平方和的差小于epsilon时，退出循环
    if(length(errorList)>2 && 
        sum(abs(rev(errorList)[1]-rev(errorList)[2])<epsilon)==1)break
}
## [1] ......省略
## [1] "iter: 926  error: 4.28051083788768"
## [1] "iter: 927  error: 4.27992351505834"
## [1] ...... 省略
## [1] "iter: 1905  error: 3.88857538358773"
## [1] "iter: 1906  error: 3.88856478602645"
## [1] "iter: 1907  error: 3.88855511510753"


error
## [1] 3.888555
W1
##            [,1]       [,2]         [,3]       [,4]       [,5]
## [1,]  1.0207210  1.3060308  0.341021202  0.4049837 -0.6806864
## [2,] -0.7807648  1.5213236 -0.314135966 -0.6190644 -0.8859370
## [3,] -0.1612614  0.6644407 -0.008990271  0.1007388 -0.1724914
## [4,] -0.2440489 -0.8276208 -0.394730430  0.1106594  1.4142867
## [5,]  0.4296572  0.8211150  0.333442902  0.7061866 -1.3624850
W2
##            [,1]         [,2]        [,3]
## [1,]  0.4091922 -0.902327264  2.54836143
## [2,] -0.2795728  0.584126143  2.63683957
## [3,] -0.1500567  0.001688003 -0.29474319
## [4,]  0.3852454 -0.387113401 -1.36236985
## [5,] -1.5131702  1.348914195 -0.05915917
W3
## [1]  1.1339223  1.4155763 -1.2045079  0.6239748  0.4274324

#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
#加载AMORE包
library(AMORE)
#建立神经网络结构，输入层有3个神经元，输入层有一个神经元
#这里加了三个隐含层，分别具有20，10，5个神经元
newNet<-newff(n.neurons=c(3,20,10,5,1),
              learning.rate.global=0.01,
              error.criterium="LMS",
              hidden.layer="sigmoid",
              output.layer="purelin",
              method="ADAPTgd")
#使用train函数，基于训练数据对神经网络进行训练
newNet.train<-train(newNet,P=vdata[,1:3],T=vdata[,4],n.shows=20,show.step=1000)
#基于现有的模型，拟合出y值，并计算残差平方和为
y<-sim(newNet.train$net,vdata[,1:3])
error<-sum((y-vdata[,4])^2)
## [1] 4.449521


#准备基础数据
vdata=iris[,1:4]
colnames(vdata)=c("x1","x2","x3","y")
#标准化x1~x3
vdata[,1:3]=scale(vdata[,1:3])
#加载RSNNS包
library(RSNNS)
#使用mlp函数，建立具有三个隐层，分别具有神经元数量为20，10，5的多层感知器网络
mlp.out<-mlp(vdata[,1:3], vdata[,4], size = c(20,10,5), maxit = 1000,
        initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
        learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2, 0),
        updateFunc = "Topological_Order", updateFuncParams = c(0),
        hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = TRUE)
vdata$pred=predict(mlp.out,vdata[,1:3])
#计算残差平方和
error=sum((vdata$y-vdata$pred)^2)
error
## [1] 3.775266


