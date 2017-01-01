#f:一元或二元运算函数
#a:第一个参数
#b:如果f是一元运算函数，则b为空，否则代表二元运算的第二个参数
g<-function(f, a, b=NULL)
{
    if(is.null(b))f(a)
    else{
        f(a, b)
    }
}


Dpow0_5<-function(x)return((x-min(x)+0.01)^0.5)
Dpow3<-function(x)return(x^3)
Dpow2<-function(x)return(x^2)
Dinv<-function(x)return(1*sign(x)/(abs(x)+0.0001))
Dadd<-function(x,y)return(x+y)
Dsub<-function(x,y)return(x-y)
Dmul<-function(x,y)return(x*y)
Ddiv<-function(x,y)return(x*sign(y)/(abs(y)+0.0001))
Dlog<-function(x) return(sign(x)*log(abs(x)+1))


g(Dmul,g(Dadd,g(sin,5),10),g(Dlog,46))
## [1] 34.80948


# 定义二元运算函数的集合
twoGroup <- c("Dadd", "Dsub", "Dmul", "Ddiv")
# 定义一元运算函数的集合
oneGroup <- c("cos", "sin", "Dlog", "Dpow0_5", "Dpow2", "Dpow3", "Dinv")
# 随机增加一元运算符
addOneGroup <- function(v, pstd = 0.3) {
    if (runif(1, 0, 1) < pstd) {
        return(paste("g(", sample(oneGroup, 1), ",<", v, ">)", sep = ""))
    } else {
        return(v)
    }
}
# 构建满二叉树，并生成数学表达式
genFullTreeExp <- function(v) {
    N <- length(v)/2
    midV <- NULL
    for (i in 1:N) {
        if (v[i] == "0" && v[i + N] != "0") {
            midV <- c(midV, paste("g(", sample(oneGroup, 1), ",<", 
            addOneGroup(v[i + N]), ">)", sep = ""))
        } else if (v[i] != "0" && v[i + N] == "0") {
            midV <- c(midV, paste("g(", sample(oneGroup, 1), ",<", 
            addOneGroup(v[i]), ">)", sep = ""))
        } else if (v[i] != "0" && v[i + N] != "0") {
            midV <- c(midV, paste("g(", sample(twoGroup, 1), ",<", 
            addOneGroup(v[i]), ">,<", addOneGroup(v[i + N]), ">)", sep = ""))
        }
    }
    if (length(midV) == 1) 
        return(addOneGroup(midV)) 
else {
        genFullTreeExp(midV)
    }
}

# 随机抽取N个特征下标
nMax = 10
N <- sample(2:nMax, 1)
# 定义原始数据集中属性的下标
featureIdx = 1:8
selFidx <- sample(c(sample(featureIdx, N, replace = T), 
                          rep(0, 2^ceiling(log(N)/log(2)) - N)))
selFidx[selFidx > 0] = paste("Xs", selFidx[selFidx > 0], sep = "")
selFidx
##  [1] "0"   "Xs3" "Xs8" "Xs5" "Xs8" "Xs3" "0"   "0"   "Xs7" "0"   "0"  
## [12] "Xs3" "0"   "Xs4" "Xs6" "0"


treeExp = genFullTreeExp(selFidx)
treeExp
## [1] "g(Ddiv,<g(Ddiv,<g(cos,<g(Dlog,<Xs7>)>)>,<g(Dsub,<Xs5>,<Xs3>)>)>,
#<g(Dsub,<g(Dinv,<g(Dinv,<Xs3>)>)>,<g(Dinv,<g(Dlog,<g(Dpow0_5,<Xs8>)>)>)>)>)"
gsub(">", "", gsub("<", "", treeExp))
## [1] "g(Ddiv,g(Ddiv,g(cos,g(Dlog,Xs7)),g(Dsub,Xs5,Xs3)),g(Dsub,g(Dinv,
## g(Dinv, Xs3)),g(Dinv,g(Dlog,g(Dpow0_5,Xs8)))))"


#构建偏二叉树，并生成数学表达式
genSideTreeExp<-function(v)
{
    #以一定的概率p，加上n(n通常为1)个一元运算符
    if(length(v)==1)return(addOneGroup(v))
    else{
        v[2]=paste("g(",sample(twoGroup,1),",<",addOneGroup(v[1]),">,<",
                         addOneGroup(v[2]),">)",sep="")
        v[1]=NA
        genSideTreeExp(as.character(na.omit(v)))
    }
}


N <- sample(2:nMax, 1)
selFidx <- sample(featureIdx, N, replace = T)
selFidx = paste("Xs", selFidx, sep = "")
selFidx
## [1] "Xs4" "Xs3" "Xs5"


treeExp = genSideTreeExp(selFidx)
treeExp
## [1] "g(Dlog,<g(Dsub,<g(Dadd,<Xs4>,<Xs3>)>,<Xs5>)>)"
gsub(">", "", gsub("<", "", treeExp))
## [1] "g(Dlog,g(Dsub,g(Dadd,Xs4,Xs3),Xs5))"


# 从原始数据特征中，随机获取表达树 
# dataName:字符串，表示所用数据集的对象
# nMax:一次最多从特征中可放回抽样次数，默认为10
randomGetTree <- function(dataName, featureIdx, nMax = 10) {
    # 1.随机抽取N个特征下标
    N <- sample(2:nMax, 1)
    # 2.随机决定是使用满二叉树还是偏二叉树
    if (sample(c(0, 1), 1) == 1) {
        #评估满二叉树，叶子节点的数量，通过生成虚拟节点，符合2^(H-1)个
        #其中H为二叉树的深度，
        # ...对生成的结果随机排序，并生成特征字符向量
      selFidx <- sample(c(sample(featureIdx, N, replace = T), 
                    rep(0, 2^ceiling(log(N)/log(2)) - N)))
      selFidx[selFidx > 0] = paste(dataName, "[,", selFidx[selFidx > 0], "]", 
            sep = "")
      treeExp = genFullTreeExp(selFidx)
    } else {
        # 构建偏二叉树，并生成数学表达式
        selFidx <- sample(featureIdx, N, replace = T)
        selFidx = paste(dataName, "[,", selFidx, "]", sep = "")
        treeExp = genSideTreeExp(selFidx)
    }
    # 3.返回二叉树表达式和适应度结果
    return(treeExp)
}


out = randomGetTree("iris", 1:4)
out
## [1] "g(Ddiv,<g(Dadd,<iris[,3]>,<iris[,2]>)>,<g(Dpow3,<iris[,1]>)>)"
tExp = gsub(">", "", gsub("<", "", out))
tExp
## [1] "g(Ddiv,g(Dadd,iris[,3],iris[,2]),g(Dpow3,iris[,1]))"
eval(parse(text = tExp))
##   [1] 0.03694 0.03740 0.04334 0.04726 0.04000 0.03556 0.04931 0.03920
##   [9] 0.05048 0.03910 0.03302 0.04521 0.03979 0.05157 0.02665 0.03186
##  省略...

#从表达式字符串，寻找子二叉树
getSubTree<-function(treeExp,dstID=NULL)
{
    library(stringr)
    #提取子集数量
    gSize<-str_count(treeExp,"<")
    #随机选取一个子集，并返回提取下标
    if(is.null(dstID))dstID<-round(runif(1,1,gSize))
    loc_start=str_locate_all(treeExp,"<")[[1]][dstID,1]
    sumPair=0
    for(i in loc_start:nchar(treeExp)){
        tchar<-substr(treeExp,start=i,stop=i)
        if(tchar=="<")sumPair=sumPair+1
        else if(tchar==">")sumPair=sumPair-1
        if(sumPair==0)break
    }
    nodeContext=substr(treeExp,start=loc_start+1,stop=i-1)
    isLeaf=is.na(str_locate(nodeContext,"<")[1,1])
    names(isLeaf)=NULL
    return(list(subStr=substr(treeExp,start=loc_start,stop=i),
        start=loc_start,end=i,nodeContext=nodeContext,isLeaf=isLeaf))
}
# 该函数用于以递归的方式获取边集、点集
# treeExp:特征表达式
# id:初始节点号，通常为1
getEdgeVR<-function(treeExp,id=1)
{
library(stringr)
    arrows=NULL
    idcount<<-idcount+1
    idList<<-c(idList,idcount)
    if(is.na(str_locate(treeExp,"<")[1,1])){
        txt=treeExp
        verNames<<-c(verNames,txt)
        isLeaf<<-c(isLeaf,1)
        return(id)
    }else{
        s0=str_locate(treeExp,",")[1,1]
        txt=substr(treeExp,start=3,stop=s0-1)
        verNames<<-c(verNames,txt)
    }
    isLeaf<<-c(isLeaf,0)
    subt=getSubTree(treeExp,1)
    arrows<-c(arrows,paste(id,"->",getEdgeVR(subt$nodeContext,id+1)))
    #若有两个子节点
    if(substr(treeExp,start=subt$end+1,stop=subt$end+1)==",")
    {
        subt2=getSubTree(substr(treeExp,start=subt$end+2,
                        stop=nchar(treeExp)),1)
        arrows<-c(arrows,paste(id,"->",getEdgeVR(subt2$nodeContext,idcount)))
    }
    return(arrows)
}
# 该函数基于特征表达式treeExp，绘制二叉树
plotTree<-function(treeExp)
{
    library(igraph)
    idcount<<-0
    idList<<-NULL
    verNames<<-NULL
    isLeaf<<-NULL
    arws=getEdgeVR(treeExp,1)
    p_vertices=data.frame(idList,verNames,isLeaf)
    p_edges<-NULL
    for(obj in arws)
    {
        tmp<-strsplit(obj," -> ")[[1]]
        tmpN<-length(tmp)
        p_edges<-rbind(p_edges,data.frame(from=tmp[1:(tmpN-1)],
to=tmp[2:tmpN]))
    }
    p_edges=p_edges[complete.cases(p_edges),]
    p_edges=unique(p_edges)
    p_vertices.color=rep("Turquoise",nrow(p_vertices))
    p_vertices.color[p_vertices$isLeaf==1]="Orange"
    gg<-graph.data.frame(d=p_edges,directed=F,vertices=p_vertices)
    plot(gg,layout=layout.reingold.tilford,
         vertex.label=as.character(p_vertices$verNames),
         vertex.label.dist=0,vertex.color=p_vertices.color,
         vertex.label.color='Maroon',vertex.label.cex=1.2)
}


plotTree('g(Ddiv,<g(Dadd,<iris[,3]>,<iris[,2]>)>,<g(Dpow3,<iris[,1]>)>)')

#产生k个个体,ksubs表示每个体对应的固定基因数量
genIndividuals<-function(k,ksubs,nMax=10)
{
    individuals=NULL
    adjusts=NULL
    for(i in 1:k)
    {
        #每个个体都从数据集的特征中产生表达树，并组合成个体
        singleTerms<-NULL
        for(j in 1:ksubs)
        {
            singleTerms<-c(singleTerms,randomGetTree(vdata,vfeatures,nMax))
        }
        individuals<-rbind(individuals,singleTerms)
        adjusts=c(adjusts,getAdjust(singleTerms))
    }
    rownames(individuals)=NULL
    individuals=data.frame(individuals,stringsAsFactors=F)
    individuals$adjusts=adjusts
    return(individuals)
}


#计算适应度：对于回归问题，通常通过计算交叉验证的误差平方和降低量作为适应度
getAdjust<-function(treeExpArray)
{
    tempData=NULL
    for(treeExp in treeExpArray)
    {
        feature=eval(parse(text=gsub('>','',gsub('<','',treeExp))))
        if(is.na(sd(feature)) || is.nan(sd(feature)) || sd(feature)==0)
        {
            feature=rep(0,NROW(feature))
        }
        tempData<-cbind(tempData,feature)
    }
    colnames(tempData)=paste("X",1:NROW(treeExpArray),sep="")
    tempData=data.frame(tempData)
    tempData$Y=vdata$Y
    newErr<-0
    for(i in 1:13)
    {
        trainData=tempData[setdiff(1:13,i),]
        testData=tempData[i,]
        newfit<-lm(Y~.,data=trainData)
        testData$newPred<-predict(newfit,testData)
        newErr<-newErr+sum(abs(testData$Y-testData$newPred)^2)
    }
    interval=stdErr-newErr
    if(interval<0)return(0)
    return(interval)
}

#对染色体进行交叉操作
interCross <-function(individuals,p,dataName)
{
    L<-nrow(individuals)
    individuals.reserve=NULL
    if(L%%2==1)
    {
        reserve_gene_id<-sample(1:L,1)
        individuals.reserve<-individuals[reserve_gene_id,]
        individuals<-individuals[setdiff(1:L,reserve_gene_id),]
        rownames(individuals)=NULL
        L<-L-1
    }
    individuals$group=sample(rep(1:(L/2),2),L)
    individuals.cross=NULL
    for(i in 1:(L/2))
    {
        #随机生成一个概率值，如果小于p，则进行交叉，否则保留原始个体进入下一代
        rand_p=runif(1,0,1)
        sub0=individuals[individuals$group==i,]
        #若发生交叉的两个个体基因型一样，那么随机生成一个与另外一个再交叉
        t0=unlist(sub0[,1:needgs])
        names(t0)=NULL
        if(length(unique(t0))==needgs)
        {
            t0=genIndividuals(1,needgs,dataName)
            t0$group=i
            sub0[1,]=t0
        }
        sub0$group=NULL
        if(rand_p<p)
        {
            individuals.cross<-rbind(individuals.cross,icross(sub0))
        }else{
            individuals.cross<-rbind(individuals.cross,sub0)
        }
    }
    individuals.cross=data.frame(individuals.cross)
    individuals.cross=rbind(individuals.cross,individuals.reserve)
    rownames(individuals.cross)=NULL
    return(individuals.cross[,1:needgs])
}
#对染色体进行交叉操作
icross<-function(subpair)
{
    subpair.all=NULL
    subpair.all<-rbind(subpair.all,subpair[1,])
    subpair.all<-rbind(subpair.all,subpair[2,])
    #从1到needgs的基因位中，随机找1~(needgs/2)个，完成基因重组
    geneLoc=sample(1:needgs,sample(1:(needgs/2),1))
    subpair.one=subpair[1,]
    subpair[1,geneLoc]=subpair[2,geneLoc]
    subpair[2,geneLoc]=subpair.one[,geneLoc]
    subpair.all<-rbind(subpair.all,subpair)
    subpair.all=data.frame(subpair.all)
    return(subpair.all)
}

A<-c("g(Dadd,<x1>,<x2>)","g(Dlog,<x1>)","g(Dadd,<g(Dlog,<x2>)>,<x3>)")
B<-c("g(sin,<x3>)","g(Dadd,<g(sin,<x1>)>,<g(Dlog,<x2>)>)","g(Dlog,<g(cos,<x4>)>)")
par(mfrow=c(2,3))
plotTree(A[1])
title("个体A-基因1")
plotTree(A[2])
title("个体A-基因2")
plotTree(A[3])
title("个体A-基因3")
plotTree(B[1])
title("个体B-基因1")
plotTree(B[2])
title("个体B-基因2")
plotTree(B[3])
title("个体B-基因3")
par(mfrow=c(1,1))

indvs<-NULL
indvs<-rbind(indvs,A)
indvs<-rbind(indvs,B)
indvs=data.frame(indvs,stringsAsFactors=F)
out=interCross(indvs,p=0.85,'vdata')[3:4,]
par(mfrow=c(2,3))
plotTree(out[1,1])
title("个体A-基因1")
plotTree(out[1,2])
title("个体A-基因2")
plotTree(out[1,3])
title("个体A-基因3")
plotTree(out[2,1])
title("个体B-基因1")
plotTree(out[2,2])
title("个体B-基因2")
plotTree(out[2,3])
title("个体B-基因3")
par(mfrow=c(1,1))

mutat<-function(individuals,p,dataName)
{
    addInd<-NULL
    for(i in 1:nrow(individuals))
    {
        if(runif(1,0,1)<p){
            geneLoc=sample(1:needgs,1)
            print(paste("第",i,"个个体<",geneLoc,"号基因>发生突变。",sep=""))
            #随机找一个基因位，并随机生成一个基因
            bak=individuals[i,]
            bak[,geneLoc]=randomGetTree(dataName,vfeatures)
            addInd<-rbind(addInd,bak)
        }
    }
    individuals<-rbind(individuals,addInd)
    rownames(individuals)=NULL
    return(individuals)
}

indvs<-NULL
indvs<-rbind(indvs,A)
indvs=data.frame(indvs,stringsAsFactors=F)
out=mutat(indvs,p=0.9,'vdata')
## [1] "第1个个体<1号基因>发生突变。"
par(mfrow=c(2,3))
plotTree(out[1,1])
title("个体A-基因1（变异前）")
plotTree(out[1,2])
title("个体A-基因2（变异前）")
plotTree(out[1,3])
title("个体A-基因3（变异前）")
plotTree(out[2,1])
title("个体A-基因1（变异后）")
plotTree(out[2,2])
title("个体A-基因2（变异后）")
plotTree(out[2,3])
title("个体A-基因3（变异后）")
par(mfrow=c(1,1))

# 读入基础数据
vdata = read.csv("C:\\Users\\haolin\\Desktop\\orgdata.csv", header = T)
# 对X1~X4进行标准化处理
vdata[, 1:4] = scale(vdata[, 1:4])
vdata
##          X1       X2      X3      X4     Y
## 1  -0.07846 -1.42369 -0.9007  1.7923  78.5
## 2  -1.09845 -1.23090  0.5044  1.3144  74.3
## 省略...

# 计算按原始特征进行交叉验证得到的误差平方和
stdErr = 0
for (i in 1:13) {
    trainData = vdata[setdiff(1:13, i), ]
    testData = vdata[i, ]
    newfit <- lm(Y ~ ., data = trainData)
    testData$newPred <- predict(newfit, testData)
    stdErr <- stdErr + sum(abs(testData$Y - testData$newPred)^2)
}
stdErr
## [1] 111.5


# 设置原始数据中的属性下标
vfeatures = 1:4
# 产生初始种群，假设种群规模为100
popSize = 100
# 设置特征长度为3
needgs = 3
individuals = genIndividuals(popSize, needgs, "vdata")
##  [1]  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000  
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [15] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [29] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [43] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [57] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [71] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [85] 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 
###         0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
##  [99] 0.00000 2.75416


alphaV<-NULL
maxVal<-NULL
ageId<-NULL
for(i in 1:100)
{
    ageId<-c(ageId,i)
    #1.交叉，先保留父代
    inter.obj<-interCross(individuals,p=0.85,'vdata')
    #2.变异，保留变异前的个体
    mutat.obj<-mutat(inter.obj,p=0.05,'vdata')
    #3.计算适应度，并选择
    adjusts<-NULL
    for(k in 1:nrow(mutat.obj))
    {
        adjusts=c(adjusts,getAdjust(unlist(mutat.obj[k,])))
    }
    mutat.obj$adjusts=adjusts
    #按adjusts排序，取前0.4*nrow(individuals)个个体进行返回
    #0.6*nrow(individuals)个随机从剩余的个体的选取
    individuals.ready=mutat.obj[order(mutat.obj$adjusts,decreasing=T),]
    topN=(round(popSize*0.4))
    individuals=individuals.ready[1:topN,]
    individuals=rbind(individuals,
          individuals.ready[sample((topN+1):length(adjusts),popSize-topN),])
    adjusts=individuals$adjusts
    alpha=max(adjusts)/mean(adjusts)
    alphaV<-c(alphaV,alpha)
    maxVal<-c(maxVal,max(individuals$adjusts))
    if(mean(adjusts)>0 && alpha<1.001){
        print(paste("进化终止，算法已收敛！共进化",i,"代！"))
        #break;
    }
}


#取individuals中第1个个体，并生成特征
treeExpArray=individuals[1,1:needgs]
tempData=NULL
for(treeExp in treeExpArray)
{
    feature=eval(parse(text=gsub('>','',gsub('<','',treeExp))))
    if(is.na(sd(feature)) || is.nan(sd(feature)) || sd(feature)==0)
    {feature=rep(0,NROW(feature))}
    tempData<-cbind(tempData,feature)
}
colnames(tempData)=paste("X",1:needgs,sep="")
tempData=data.frame(tempData)
tempData$Y=vdata$Y
head(tmpData)
##            X1          X2          X3     Y
## 1 -1.40214939 -0.04481223 -1.44205617  78.5
## 2 -1.87902769 -0.11870397 -2.23217894  74.3
## 3  2.49784605 -0.08116145 -0.29032231 104.2
## 4  0.07476716 -0.14536888  0.57241276  87.6
## 5  1.46518565  0.55157515 -0.01928659  95.9
## 6  2.41030598 -0.02737613 -1.03601553 109.2
par(mfrow=c(1,3))
plotTree(treeExpArray[1,1])
title("特征-X1")
plotTree(treeExpArray[1,2])
title("特征-X2")
plotTree(treeExpArray[1,3])
title("特征-X3")
par(mfrow=c(1,1))

