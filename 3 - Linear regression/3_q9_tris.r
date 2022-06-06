rm(list=ls())

n<-3 # number input variables
p<-n+1
N=12

X<-cbind(c(-1,-0.5,0.5,1,-1,-0.5,0.5,1,-1,-0.5,0.5,1 ),
         c(-1,-1,-1,-1,0,0,0,0,1,1,1,1 ),
         c(-1,-1,-1,-1,-1,-1,1,1,1,1,1,1 ))
X<-cbind(array(1,c(N,1)),X)

beta<-c(1,0.1,-0.1,1)
R<-10000
sd.w<-1

xhat=array(c(1,1,1,1),c(p,1))
beta.hat<-array(0,c(p,R))
Y.hat<-NULL
for (r in 1:R){
  Y<-X%*%beta+rnorm(N,sd=sd.w)
  beta.hat[,r]<-solve(t(X)%*%X)%*%t(X)%*%Y
  Y.hat<-c(Y.hat,t(xhat)%*%beta.hat[,r])
}

print(paste("Prediction Var[y]:", sd.w^2*(t(xhat)%*%solve(t(X)%*%X)%*%xhat)))
print(paste("Bootstrap estimate Var[y]:", var(Y.hat)))
