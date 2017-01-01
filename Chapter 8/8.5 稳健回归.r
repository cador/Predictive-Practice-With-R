vdata = iris[, 1:4]
colnames(vdata) = c("x1", "x2", "x3", "y")
# 对x1~x4，随机替换15个较大值
vdata$x1[sample(1:150, 5)] = runif(5, 10, 30)
vdata$x2[sample(1:150, 5)] = runif(5, 10, 30)
vdata$x3[sample(1:150, 5)] = runif(5, 10, 30)
################ 迭代法求解##################
beta = lm.obj$coefficients
c = 0.8
k = 1
while (k < 1000) {
    y0 = beta[1] + vdata$x1 * beta[2] + vdata$x2 * beta[3] + vdata$x3 * beta[4]
    epsion = vdata$y - y0
    delta = sd(epsion)
    epsion = epsion/delta
    faik = rep(0, 4)
    faikD = diag(rep(0, 4))
    for (i in 1:length(y0)) {
        if (abs(epsion[i]) <= c) {
            xi = cbind(1, as.matrix(vdata[i, 1:3]))
            faik = faik + sin(epsion[i]/c) * t(xi)
            faikD = faikD + (-1/(delta * c)) * cos(epsion[i]/c) * 
                    tcrossprod(t(xi), t(xi))
        }
    }
    k = k + 1
    b = solve(faikD) %*% faik
    beta = beta - b
    print(max(abs(t(b))))
    if (max(abs(t(b))) < 1e-15) 
        break
}
## [1] 0.466
## [1] 0.5393
## [1] 0.02284
## [1] 3.394e-05
## [1] 4.631e-09
## [1] 2.965e-12
## [1] 1.966e-15
## [1] 1.264e-16
y0 = beta[1] + vdata$x1 * beta[2] + vdata$x2 * beta[3] + vdata$x3 * beta[4]
epsion = vdata$y - y0
mean(abs(epsion))
## [1] 0.3659
beta
##    -0.341796
## x1 -0.008648
## x2  0.002321
## x3  0.422969

lm.obj <- lm(y ~ x1 + x2 + x3, data = vdata)
beta = lm.obj$coefficients
beta
## (Intercept)          x1          x2          x3 
##     0.64056     0.04126    -0.02209     0.08971
y0 = beta[1] + vdata$x1 * beta[2] + vdata$x2 * beta[3] + vdata$x3 * beta[4]
epsion = vdata$y - y0
mean(abs(epsion))
## [1] 0.5342

# 对基础数据进行转换
vdata = iris[, 1:4]
colnames(vdata) = c("x1", "x2", "x3", "y")
# 对x1~x4，随机替换15个较大值
vdata$x1[sample(1:150, 5)] = runif(5, 10, 30)
vdata$x2[sample(1:150, 5)] = runif(5, 10, 30)
vdata$x3[sample(1:150, 5)] = runif(5, 10, 30)
library(MASS)
rlm.obj <- rlm(y ~ x1 + x2 + x3, data = vdata, method = "M")
rlm.obj
## Call:
## rlm(formula = y ~ x1 + x2 + x3, data = vdata, method = "M")
## Converged in 15 iterations
## 
## Coefficients:
## (Intercept)          x1          x2          x3 
##  -0.1718824  -0.0001233  -0.0019438   0.3577231 
## 
## Degrees of freedom: 150 total; 146 residual
## Scale estimate: 0.189
beta = rlm.obj$coefficients
y0 = beta[1] + vdata$x1 * beta[2] + vdata$x2 * beta[3] + vdata$x3 * beta[4]
epsion = vdata$y - y0
sum(abs(epsion))
## [1] 52.39
lqs.obj <- lqs(y ~ x1 + x2 + x3, data = vdata, method = "S", model = T)
lqs.obj
## Call:
## lqs.formula(formula = y ~ x1 + x2 + x3, data = vdata, method = "S", 
##     model = T)
## 
## Coefficients:
## (Intercept)           x1           x2           x3  
##   -2.79e-01    -2.48e-03    -7.86e-05     3.79e-01  
## 
## Scale estimates 0.179
beta = lqs.obj$coefficients
y0 = beta[1] + vdata$x1 * beta[2] + vdata$x2 * beta[3] + vdata$x3 * beta[4]
epsion = vdata$y - y0
sum(abs(epsion))
## [1] 52.24
