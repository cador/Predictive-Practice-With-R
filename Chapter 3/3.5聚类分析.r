#重构数据
ap.data<-t(mapply(function(i){AirPassengers[(12*(i-1)+1):(i*12)]},1:12))
#标准化曲线
ap.data.std<-t(apply(ap.data,1,function(x){(x-min(x))/(max(x)-min(x))}))
#使用kmeans函数进行聚类，假定分成两类
kOut<-kmeans(ap.data.std,centers=2,nstart=20,iter.max=200)
#聚类统计情况如下
table(kOut$cluster)

#设置类标签
ap.data.std<-cbind(ap.data.std,kOut$cluster)
#画出曲线图
plot(1:12,1:12,col='white',ylim=c(0,1),xlab="年份",ylab="标准化值")
for(i in 1:nrow(ap.data.std))
{
    lines(1:12,ap.data.std[i,1:12],col=ap.data.std[i,13]+2)
}
legend(1,1,c("类1","类2"),lty=1,col=c("blue","green "))

#重构数据
ap.data<-t(mapply(function(i){AirPassengers[(12*(i-1)+1):(i*12)]},1:12))
#标准化曲线
ap.data.std<-t(apply(ap.data,1,function(x){(x-min(x))/(max(x)-min(x))}))
#使用hclust函数进行聚类
hc<-hclust(dist(ap.data.std),method="ward.D2")
plot(hc)
rect.hclust(hc,k=2)

#设置类标签
ap.data.std<-cbind(ap.data.std,cutree(hc,k=2))
#画出曲线图
plot(1:12,1:12,col='white',ylim=c(0,1))
for(i in 1:nrow(ap.data.std))
{
    lines(1:12,ap.data.std[i,1:12],col=ap.data.std[i,13]+2)
}

