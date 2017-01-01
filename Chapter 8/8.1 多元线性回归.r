# 从文件中加载M数据集
data = read.csv("f:\\mm.csv")
colnames(data) = c("y", "x1", "x2")
library(car)
vif(lm(y ~ x1 + x2, data))
##    x1    x2 
## 35.96 35.96

