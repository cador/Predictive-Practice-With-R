minMSE=1000
f_a0=NULL
f_b0=NULL
f_c0=NULL
f_d0=NULL
k=55
for(a in 0:k)
{
    for(b in 0:k)
    {
        for(c in 0:k)
        {
            for(d in 0:k)
            {
                #参数实例化
                a0<-(-1)+2*a/k
                b0<-(-1)+2*b/k
                c0<-(-1)+2*c/k
                d0<-(-1)+2*d/k
                #计算均方误差
                y0<-a0+b0*iris$Sepal.Length+c0*iris$Sepal.Width+
                    d0*iris$Petal.Length
                mse<-mean((iris$Petal.Width-y0)^2)
                if(mse<minMSE)
                {
                    minMSE<-mse
                    f_a0<-a0
                    f_b0<-b0
                    f_c0<-c0
                    f_d0<-d0
                }
            }
        }
    }
}
print(minMSE)
## [1] 0.03607967
print(c(f_a0,f_b0,f_c0,f_d0))
## [1] -0.3454545 -0.2000000  0.2363636  0.5272727
