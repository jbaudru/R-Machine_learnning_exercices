rm(list=ls())
load("EXAM.Rdata")

D=Q5.G1.D
Ip=which(D[,3]==1)
In=which(D[,3]==-1)

classes=c("-1","1")
N=NROW(D)
n=2
X=D[,1:n]

plot(X[Ip,1],X[Ip,2],col="green")
points(X[In,1],X[In,2],col="red")


Pp=length(Ip)/N
Pn=1-Pp
Sigmap=var(X[Ip,])
Sigman=var(X[In,])

mup=apply(X[Ip,],2,mean)
mun=apply(X[In,],2,mean)

Xts=rbind(c(0,0),c(1,1),c(-1,-1),c(2,2))

for (i in 1:NROW(Xts)){
  x=Xts[i,]
  gp=-0.5*t(x-mup)%*%solve(Sigmap)%*%(x-mup)-n/2*log(2*pi)-0.5*log(det(Sigmap))+log(Pp)
  gn=-0.5*t(x-mun)%*%solve(Sigman)%*%(x-mun)-n/2*log(2*pi)-0.5*log(det(Sigman))+log(Pn)
  cat("Prediction in x=", x, " is ",classes[which.max(c(gn,gp))],"\n")
}
