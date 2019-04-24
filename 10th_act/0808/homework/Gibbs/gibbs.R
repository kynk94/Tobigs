rm(list=ls())
graphics.off()

library(mnormt)
set.seed(1)
#f(x1,x2)=2exp(-x1*x2-x1-x2), x1>0, x2>0
#f(x1)=2/((x1+1)(exp(x1)))
#f(x2)=2/((x2+1)(exp(x2)))
#f(x1,x2)/f(x1) = exp(-x1*x2-x2)(x1+1) lambda = x1+1 
#f(x1,x2)/f(x2) = exp(-x1*x2-x1)(x2+1) lambda = x2+1
#E(x)=integral 0 to inf xf(x) dx = 2+2e*Ei(-1) = 0.8073052753536 = E(x1) = E(x2)
#V(x)=integral 0 to inf ((x-E(x))^2)*f(x) dx   = 0.66654 = V(x1) = V(x2)
#E = 0.8073052753536
#V = 0.66654
while(1){
x1=seq(0,4,0.1)
x2=seq(0,4,0.1)
n=length(x1)
z=matrix(0,n,n)
for (i in 1:n){
  z[i,]=2*exp(-x1[i]*x2-x1[i]-x2)
}
contour(x1,x2,z,levels=c(0.005,0.05,0.1,0.15),lwd=2,main="Contour")
Nsim=10^3
x1=numeric(Nsim)
x2=numeric(Nsim)
x1[1]=10^(-1)
x2[1]=10^(-1)
for (i in 2:Nsim){
  x1[i]=rexp(1,x2[i-1]+1)
  x2[i]=rexp(1,x1[i]+1)
}
# for (i in 1:Nsim){
#   segments(x1[i],x2[i],x1[i+1],x2[i],lwd=2,col="blue")
#   segments(x1[i+1],x2[i],x1[i+1],x2[i+1],lwd=2,col="blue")
# }

points(x1,x2,pch=20,col="green")
break
}