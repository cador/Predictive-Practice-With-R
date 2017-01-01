#将AirPassengers数据转换成环比值
L<-NROW(AirPassengers)
ap.chain<-AirPassengers[2:L]/AirPassengers[1:(L-1)]
plot(ap.chain,pch=20)
lines(ap.chain)
abline(h=1.0,lty=2,col='red')

ap.chain.dez<-cut(ap.chain,breaks=4,include.lowest=T)
ap.chain.lab<-cut(ap.chain,breaks=4,include.lowest=T,labels=c("A","B","C","D"))
out<-data.frame(dez=ap.chain.dez,lab=ap.chain.lab,chain=ap.chain)
head(out)

#构建数据集
winSize=10
conMatrix=t(mapply(function(i)ap.chain.lab[i:(i+winSize-1)],
                                 1:(NROW(ap.chain)-winSize+1)))
#由于数据都是按时间先后顺序整理的，因此可用前80%提取规则，用后20%验证规则
partVal<-round(dim(conMatrix)[1]*0.8,0)
trainData<-data.frame(conMatrix[1:partVal,])
validData<-conMatrix[(partVal+1):dim(conMatrix)[1],]
#使用Apriori算法，提取关联规则
library(arules)
apriori.obj<-apriori(trainData,parameter=list(supp=0.1,conf=0.5,target="rules"),appearance=list(rhs=c("X10=A","X10=B","X10=C","X10=D"),default="lhs"))
inspect(apriori.obj)

inspect(apriori.obj[which.max(quality(apriori.obj)$lift)])
##   lhs            rhs     support   confidence lift    
## 6 {X4=B,X9=A} => {X10=A} 0.1028037 0.9166667  4.264493
rulesData=as(apriori.obj,"data.frame")
rulesData[which.max(rulesData$lift),]

tmp<-validData[validData[,4]=='B' & validData[,9]=='A',10]
hitRate<-paste(NROW(tmp=='A')/NROW(tmp)*100,"%",sep="")

hitRate

#该函数使用apriori函数提取的规则对象，结合igraph绘制网络图
#apriori.obj:apriori函数提取的规则对象
#layoutType:通常设置为layout.fruchterman.reingold、layout.graphopt
arule.graph.plot<-function(apriori.obj,layoutType)
{
    #1.将apriori.obj转换成数据框
    rules.data<-as(apriori.obj,"data.frame")
    rules.data<-cbind(rules.data,t(apply(rules.data,1,function(x)
                                 strsplit(x[1],"\\} => \\{")[[1]])))
    rules.data$rules<-NULL
    rules.data$LHS<-apply(rules.data,1,function(x)
                                 strsplit(as.character(x[4]),"\\{")[[1]][2])
    rules.data$RHS<-apply(rules.data,1,function(x)
                                 strsplit(as.character(x[5]),"\\}")[[1]][1])
    rules.data<-rules.data[,c("support","confidence","LHS","RHS")]
    rules.data<-rules.data[!is.na(rules.data$LHS),]
    #2.对每条规则进行处理，得到网络图中的点和边
    edge.prefix<-NULL
    edge.suffix<-NULL
    vertix.name<-NULL
    vertix.size<-NULL
    vertix.col<-NULL
    for(i in 1:nrow(rules.data))
    {
        lhsItems<-strsplit(rules.data[i,'LHS'],",")[[1]]
        for(lhsItem in lhsItems)
        {
            edge.prefix<-c(edge.prefix,lhsItem)
            edge.suffix<-c(edge.suffix,row.names(rules.data)[i])
            vertix.name<-c(vertix.name,lhsItem)
            vertix.size<-c(vertix.size,0)
            vertix.col<-c(vertix.col,0)
        }
        rhsItems<-strsplit(rules.data[i,'RHS'],",")[[1]]
        for(rhsItem in rhsItems)
        {
            edge.prefix<-c(edge.prefix,row.names(rules.data)[i])
            edge.suffix<-c(edge.suffix,rhsItem)
            vertix.name<-c(vertix.name,rhsItem)
            vertix.size<-c(vertix.size,0)
            vertix.col<-c(vertix.col,0)
        }
        vertix.name<-c(vertix.name,row.names(rules.data)[i])
        vertix.size<-c(vertix.size,rules.data[i,'support'])
        vertix.col<-c(vertix.col,rules.data[i,'confidence'])
    }
    edges<-data.frame(edge.prefix,edge.suffix,stringsAsFactors=F)
    vertices<-data.frame(vertix.name,vertix.size,vertix.col,stringsAsFactors=F)
    vertices<-vertices[!duplicated(vertices),]
    vertices<-vertices[order(vertices$vertix.col,decreasing=T),]
    n_ves<-nrow(vertices[vertices$vertix.size>0,])
    vertices[vertices$vertix.size>0,]$vertix.col<-heat.colors(n_ves)
    vertices[vertices$vertix.size==0,]$vertix.col<-"#3399FF"
    vertices$vertix.size=vertices$vertix.size*100
    vertices[vertices$vertix.size==0,]$vertix.size=10
    #3.使用igraph建立网络图
    library(igraph)
    gg <- graph.data.frame(d = edges, directed = T, vertices = vertices)
    plot(gg,vertex.color=vertices$vertix.col,vertex.size=vertices$vertix.size,
         edge.arrow.size=0.5,vertex.label.color="#000000",vertex.label.cex=0.8,
         layout=layoutType)
}

arule.graph.plot(apriori.obj,layout.fruchterman.reingold)


#根据示例数据，手动输入构建包含事务信息的list对象
alist=list(c('A','D'),
           c('A','E','F'),
           c('A','B','C','E','F'),
           c('B','C','D'),
           c('A','C','D','E','F'),
           c('A','B','D','F')
)
#将alist强制转换成transactions对象
trans1 <- as(alist, "transactions")
#加载arules包，使用eclat函数提取频繁项集，最小支持度设置为0.5
library(arules)
items=eclat(trans1,parameter=list(support = 0.5),control=list(verbose=FALSE))  
#查看提取的频繁项集
inspect(items)
##    items   support  
## 1  {A,E,F} 0.5000000
## 2  {A,E}   0.5000000
## 3  {E,F}   0.5000000
## 4  {A,D}   0.5000000
## 5  {A,F}   0.6666667
## 6  {A}     0.8333333
## 7  {F}     0.6666667
## 8  {D}     0.6666667
## 9  {E}     0.5000000
## 10 {C}     0.5000000
## 11 {B}     0.5000000
items.data<-as(items,'data.frame')
items.data<-items.data[order(items.data$support,decreasing=T),]
a=items.data$support
names(a)=items.data$items
library("RColorBrewer")
barplot(a,ylim=c(0,1),col=brewer.pal(11,"RdYlBu"))


library(arulesSequences)
data(zaki)
s0<-cspade(zaki,parameter=list(support=0.5),control=list(verbose=TRUE))

#将序列模式转换成数据框
as(s0,'data.frame')

