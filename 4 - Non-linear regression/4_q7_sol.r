rm(list=ls())

N=20
R=10000

Yhat=NULL
Yhat2=NULL
for (r in 1:R){
  x=runif(N,-1,1)
  Y=array(sin(x)+rnorm(N,0,sqrt(0.5)),c(N,1))
  X=cbind(numeric(N)+1,x)
  betahat=solve(t(X)%*%X) %*%t(X)%*%Y
  xq=array(c(1,0),c(2,1))
  Yhat=c(Yhat,t(xq)%*%betahat)
  X2=cbind(numeric(N)+1,x,x^2)
  betahat2=solve(t(X2)%*%X2) %*%t(X2)%*%Y
  xq=array(c(1,0,0),c(3,1))
  Yhat2=c(Yhat2,t(xq)%*%betahat2)
}

cat("Var yhat=",var(Yhat), "\n Bias^2 yhat=", (mean(Yhat)-sin(0))^2,"\n")
cat("Var yhat2=",var(Yhat2), "\n Bias^2 yhat2=", (mean(Yhat2)-sin(0))^2,"\n")
