#自定义目标函数C
C<-function(s){
    1/(s*sin(s)+12)
}
#1、初始化
stmp=NULL
for(i in 1:100)stmp<-c(stmp,runif(1,0,12.55))
t0<-var(C(stmp))       #设定初始温度
s0<-runif(1,0,12.55)   #设定初始解状态
iters<-3000            #设定迭代次数
ccnt<-200              #设定终止条件，连续ccnt个新解都没有接受时终止算法
hisbest<-12.55         #保存历史最好的状态，默认取上边界值
ccntVc<-NULL
for(t in 1:iters)
{
    #在s0附近，产生新解，但又能包含定义内的所有值
    s1<-rnorm(1,mean=s0,sd=2)
    while(s1<0 || s1>12.55){
        s1<-rnorm(1,mean=s0,sd=3)
    }
    #计算能量增量
    delta_t<-C(s1)-C(s0)
    if(delta_t<0)
    {
        s0<-s1
        ccntVc<-c(ccntVc,1)
    }else{
        p=exp(-delta_t/t0)
        if(runif(1,0,1)<p)
        {
            s0<-s1
            ccntVc<-c(ccntVc,1)
        }else{
            ccntVc<-c(ccntVc,0)
        }
    }
    hisbest<-ifelse(C(s1)<C(hisbest),s1,hisbest)
    hisbest<-ifelse(C(s0)<C(hisbest),s0,hisbest)
    #更新温度
    t0<-t0/log(1+t)
    #检查终止条件
    if(NROW(ccntVc)>ccnt && sum(ccntVc[(NROW(ccntVc)-ccnt+1):NROW(ccntVc)])==0)
    {
        print(paste("连续",ccnt,"次没有接受新解，算法终止！"))
        break;
    }
}
## [1] "连续 200 次没有接受新解，算法终止！"
#状态最终停留位置
print(s0)
## [1] 7.981894
#迭代次数
print(t)
## [1] 656
#最佳状态，即对应最优解的状态
print(hisbest)
## [1] 7.981894

library(GenSA)
gensa0<-GenSA(par=runif(1,0,12.55),fn=function(s)1/(s*sin(s)+12),lower=c(0),
              upper=c(12.55))
str(gensa0)
## List of 4
##  $ value    : num 0.0502
##  $ par      : num 7.98
##  $ trace.mat: num [1:9992, 1:4] 1 1 2 2 3 3 4 4 5 5 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:4] "nb.steps" "temperature" "function.value" 
##                      "current.minimum"
##  $ counts   : int 35892

opt0<-optim(par=runif(1,0,12.55),fn=function(s){
    if(s<0 || s>12.55)
    {
        return(exp(100))
    }else{
        return(1/(s*sin(s)+12))
    }
},method="SANN")
opt0
## $par
## [1] 7.978569
## 
## $value
## [1] 0.05020905
## 
## $counts
## function gradient 
##    10000       NA 
## 
## $convergence
## [1] 0
## 
## $message
## NULL

