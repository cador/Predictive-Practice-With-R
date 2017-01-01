library(rgl)
x <- (-100:100)  
y <- (-100:100)  
z=matrix(mapply(function(i){mapply(function(v0){return(sqrt(i^2+v0^2))},y)},x),
         nrow=201,byrow=T)
open3d()
surface3d(x,y,z,back = "lines",color=terrain.colors(z^0.2))

#1.初始化粒子群（包含20个粒子）
vmax<-30
pbest<-NULL
gbest<-NULL
gbest.add<-NULL
w<-1 #设置惯性权重
c1<-c2<-2 #设置加速度常数
iters<-1000#设置最大迭代次数
alpha<-0.001#设置最佳适应度值的增量阈值
#--在给定定义域内，随机生成位置矩阵如下
xMat<-matrix(c(x=runif(20,-100,100),y=runif(20,-100,100)),byrow=F,ncol=2,
             dimnames=list(NULL,c("x","y")))
#--在给定最大速度的限制下，随机生成速度矩阵如下
vMat<-matrix(c(x=runif(20,-vmax,vmax),y=runif(20,-vmax,vmax)),byrow=F,ncol=2,
             dimnames=list(NULL,c("x","y")))

#2.计算种群中所有粒子的适应度
adjusts<-apply(xMat,1,function(v){1/sqrt(sum(v^2)+1)})

#3.更新pbest、gbest，同时更新所有粒子的位置与速度
pbest<-xMat
pbest<-cbind(pbest,adjusts)
gbest<-pbest[which.max(pbest[,3]),]
for(k in 1:iters)
{
    #---更新pbest
    #遍历adjusts，如果对应粒子的适应度是历史中最高的，则完成替换
    mapply(function(no,adj){
        if(adj>pbest[no,3])
        {
            pbest[no,]<<-c(xMat[no,],adj)
            #print("--更新pbest")
        }
    },1:length(adjusts),adjusts)
    #print("-----------pbest-----------")
    #print(pbest)
    #---更新gbest
    if(max(pbest[,3])>gbest[3]){
        gbest.add<-max(pbest[,3])-gbest[3]
        gbest<-pbest[which.max(pbest[,3]),]
        print("--更新gbest")
        print(gbest.add)
    }
    #print("-----------gbest-------------")
    #print(gbest)
    #画出对应位置的点
    plot(xMat[,1],xMat[,2],pch=20,col='blue',xlim=c(-100,100),ylim=c(-100,100))
    points(gbest[1],gbest[2],pch=8,col='red')
    points(0,0,pch=20,cex=0.5)
    points(0,0,pch=21,cex=2)
    dev.off()
    #--更新所有粒子的位置与速度
    old.xMat<-xMat
    xMat<-xMat+vMat
    vMat<-w*vMat+c1*runif(1,0,1)*(pbest[,1:2]-old.xMat)+c2*runif(1,0,1)*
               (matrix(rep(gbest[1:2],20),ncol=2,byrow=T)-old.xMat)
    #----如果vMat有值超过了边界值，则设定为边界值
    vMat[vMat<(-vmax)]<-(-vmax)
    vMat[vMat>vmax]<-vmax
    #计算更新后种群中所有粒子的适应度
    adjusts<-apply(xMat,1,function(v){1/sqrt(sum(v^2)+1)})
    #检查全局适应度的增量，如果小于0.0002，则算法停止
    if(!is.null(gbest.add) && gbest.add<0.0002){
        print(paste("k=",k,"算法结束！"))
        break;
    } 
}
## [1] "--更新gbest"
##      adjusts 
## 0.0009462894
## [1] "--更新gbest"
##   adjusts 
## 0.0399164
#省略输出
## [1] "k= 531 算法结束！"
#最佳适应度对应的位置
gbest[1:2]
##           x           y 
##  0.01592260 -0.01290851

library(pso)
psoObj<-psoptim(rep(NA,2),function(x)sqrt(x[1]^2+x[2]^2),lower=c(-100,-100),
                upper=c(100,100),control=list(s=50))
psoObj
## $par
## [1] -1.793683e-49 -2.662434e-49
## 
## $value
## [1] 3.210273e-49
## 
## $counts
##  function iteration  restarts 
##     50000      1000         0 
## 
## $convergence
## [1] 2
## 
## $message
## [1] "Maximal number of iterations reached"

