#对染色体进行交叉操作
icross<-function(str0,str1)
{
    k<-sample(2:(nchar(str0)-1),1)
    out0<-paste(substring(str0,1,k-1),substring(str1,k),sep="")
    out1<-paste(substring(str1,1,k-1),substring(str0,k),sep="")
    return(list(out0,out1))
}
interCross <-function(vgene,p)
{
    L<-length(vgene)
    reserve_gene=NULL
    if(L%%2==1)
    {
        idx<-sample(1:L,1)
        reserve_gene<-vgene[idx]
        p1=NULL
        p2=NULL
        if(idx>1 && idx<L)
        {
            p1=vgene[1:(idx-1)]
            p2=vgene[(idx+1):L]
        }else if (idx==1){
            p2=vgene[(idx+1):L]
        }else if(idx==L){
            p1=vgene[1:(idx-1)]
        }
        vgene=c(p1,p2)
        L<-L-1
    }
    t0=data.frame(vgene)
    t0$vgene=as.character(t0$vgene)
    t0$group=sample(rep(1:(L/2),2),L)
    vgene=unlist(mapply(function(x){
        #随机生成一个概率值，如果小于p，则进行交叉，否则保留原始个体进入下一代
        rand_p=runif(1,0,1)
        sub0=t0[t0$group==x,]$vgene
        #若发生交叉的两个个体基因型一样，那么随机生成一个与另外一个再交叉
        if(sub0[1]==sub0[2])
        {
            sub0[1]=paste(sample(c(0,1),11,replace=T),collapse="")
        }
        if(rand_p<p)
        {
            return(icross(sub0[1],sub0[2]))
        }else{
            return(list(sub0[1],sub0[2]))
        }
    },1:(L/2)))
    return(c(reserve_gene,vgene))
}
#基因突变
mutat_ele <-function(dst)
{
    k<-sample(1:nchar(dst),1)
    p1=NULL
    p2=ifelse(substring(dst,k,k)=="0",1,0)
    p3=NULL
    if(k==1){
        p3=substring(dst,k+1,nchar(dst))
    }else if(k==nchar(dst)){
        p1=substring(dst,1,k-1)
    }else{
        p1=substring(dst,1,k-1)
        p3=substring(dst,k+1,nchar(dst))
    }
    dst2<-paste(p1,p2,p3,sep="")
    return(dst2)
}
mutat <- function(vgene,p)
{
    L<-length(vgene)
    for(i in 1:L)
    {
        #随机生成一个概率值，如果小于p，则进行变异
        rand_p=runif(1,0,1)
        if(rand_p<p){
            dst<-vgene[i]
            dst2<-mutat_ele(dst)
            print(paste("第",i,"个个体发生突变。",sep=""))
            print(paste("原染色体：",dst,sep=""))
            print(paste("突变后的染色体：",dst2,sep=""))
            vgene[i]<-dst2
        }
    }
    return(vgene)
}
#计算适应度
getX<-function(vgene)
{
    vgene <- strtoi(vgene,base=2)
    vgene <- vgene*12.55/(2^11-1)
    return(vgene)
}
getAdjust <- function(vgene)
{
    vgene <- getX(vgene)
    return(vgene*sin(vgene)+12)
}

#通过轮盘赌来进行选择
cycleSel <- function(vgene)
{
    L<-length(vgene)
    t0=data.frame(vgene)
    t0$vgene=as.character(t0$vgene)
    t0$adjust=getAdjust(t0$vgene)
    t0$prob=t0$adjust/sum(t0$adjust)
    cu_prob=NULL
    for(i in 1:L)
    {
        cu_prob=c(cu_prob,sum(t0$prob[1:i]))
    }
    t0$cu_prob=cu_prob
    #随机产生L个0~1的随机数
    r0=runif(L,0,1)
    #print(r0)
    sel0=mapply(function(x){
        tmp0=which(x>cu_prob)
        if(length(tmp0)==0) return(1)
        else return(max(tmp0)+1)
    },r0)
    #将选择列表中，适应度最低的移除
    sel0<-sel0[-which(sel0==sel0[which.min(t0[sel0,]$adjust)])]
    #将种群中适应度最高的个体加入选择列表中
    sel0<-c(sel0,as.integer(rownames(t0[order(t0$adjust,decreasing = T),
                                       ][1:(L-length(sel0)),])))
    excludes<-setdiff(1:L,sel0)
    selData=t0[sel0,-c(3,4)]
    rownames(selData)=NULL
    return(list(select=sel0,exclude=excludes,selectData=selData))
}
#读入原始数据，第一个属性为NO，表示个体编号，第二个属性为GENE，为个体的基因型
tmp=read.csv('f:\\tmp.csv',header=T,sep='\t',colClasses = c("integer","character"),col.names = c("NO","GENE"))
#从原始种群中，选择适应度较好的个体，进行交叉变异
rgene=tmp$GENE
L_1=NULL
L_2=NULL
L_3=NULL
L_4=NULL
for(i in 1:100)
{
    L_1=c(L_1,i)
    print(paste("-----------",i,"------------"))
    #1、选择
    out3=cycleSel(rgene)
    #2、交叉
    out4=interCross(out3$selectData$vgene,p=0.85)
    #3、变异
    rgene=mutat(out4,p=0.05)
    t000=rgene[which.max(getAdjust(rgene))]
    L_2=c(L_2,t000)
    L_3=c(L_3,strtoi(t000,base=2)*12.55/(2^11-1))
    L_4=c(L_4,max(getAdjust(rgene)))
    #保存图片
    png(paste("C:/Users/haolin/Desktop/123/",i,".png",sep=""),width=600,
        height=600)
    x=(0:(4*pi*100))/100
    y=x*sin(x)
    plot(x,y,col='white',main=paste('第',i,'代'))
    lines(x,y,col='blue')
    x=getX(rgene)
    y=x*sin(x)
    points(x,y,pch=8,col='red')
    dev.off()
    alpha=max(getAdjust(rgene))/mean(getAdjust(rgene))
    if(alpha<1.001){
        print(paste("进化终止，算法已收敛！共进化",i,"代！"))
        break;
    }
}
#定义适应度函数
getAdjust <- function(x)
{
    if(x>=0 && x<=12.55)
    {
        return(-(x*sin(x)))
    }else{
        return(exp(100))
    }
}

library(mcga)
m<-mcga(popsize = 20, chsize = 1, minval = 0, maxval = 12.55 , maxiter = 1000, 
        evalFunc = getAdjust)
str(m)
## List of 10
##  $ population: num [1:20, 1] 7.98 7.98 7.98 7.98 7.98 ...
##  $ costs     : num [1:20] -7.92 -7.92 -7.92 -7.92 -7.92 ...
##  $ popsize   : num 20
##  $ chsize    : num 1
##  $ crossprob : num 1
##  $ mutateprob: num 0.01
##  $ elitism   : num 1
##  $ minval    : num 0
##  $ maxval    : num 12.6
##  $ maxiter   : num 1000

library(genalg)
#定义适应度函数
getAdjust <- function(x)
{
    if(x>=0 && x<=12.55)
    {
        return(-(x*sin(x)))
    }else{
        return(exp(100))
    }
}

#定义监控函数
monitor<-function(rbga0)
{
    #打印种群中第一个个体的值population[1,]
    print(rbga0$population[1,])
}

rbgaObj<-rbga(stringMin = c(0), stringMax = c(12.55), popSize = 100, 
              iters = 1000, mutationChance = 0.01, monitorFunc = monitor, 
              evalFunc = getAdjust, verbose = TRUE)
## Testing the sanity of parameters...
## Not showing GA settings...
## Starting with random values in the given domains...
## Starting iteration 1 
## Calucating evaluation values... ............................................
##  ........................................................ done.
## Sending current state to rgba.monitor()...
## [1] 3.441201
## Creating next generation...
##   sorting results...
##   applying elitism...
##   cannot crossover (#vars=1), using new randoms...
##   applying mutations... 2 mutations applied
## Starting iteration 2 
## Calucating evaluation values... ............................................
##  .................................... done.
## Sending current state to rgba.monitor()...
## [1] 8.049333

#省略输出
#查看rbgaObj对象的结构
str(rbgaObj)
## List of 12
##  $ type          : chr "floats chromosome"
##  $ stringMin     : num 0
##  $ stringMax     : num 12.6
##  $ popSize       : num 100
##  $ iters         : num 1000
##  $ suggestions   : NULL
##  $ population    : num [1:100, 1] 7.98 7.98 7.98 7.98 7.98 ...
##  $ elitism       : num 20
##  $ mutationChance: num 0.01
##  $ evaluations   : num [1:100] -7.92 -7.92 -7.92 -7.92 -7.92 ...
##  $ best          : num [1:1000] -7.9 -7.9 -7.9 -7.9 -7.9 ...
##  $ mean          : num [1:1000] 0.792 -0.491 -1.757 -2.761 -4.038 ...
##  - attr(*, "class")= chr "rbga"

#绘制最佳与平均评估值
plot(rbgaObj)

#绘制直方图
plot(rbgaObj,type="hist",breaks=50)

#绘制参数图
plot(rbgaObj,type="vars")
