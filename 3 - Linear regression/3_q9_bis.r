library(MASS)

n<-2 # number input variables
p<-n+1
N=12
X<-cbind(c(-1,-0.5,0.5,1,-1,-0.5,0.5,1,-1,-0.5,0.5,1 ),c(-1,-1,-1,-1,0,0,0,0,1,1,1,1 ) )
X<-cbind(array(1,c(N,1)),X)
beta<-c(1,0.1,-0.1)
R<-10000
sd.w<-1

xhat=array(c(1,0,0),c(3,1))
beta.hat<-array(0,c(p,R))

#Y.hat<-NULL
B0<-NULL
B1<-NULL
B2<-NULL
for (r in 1:R){
  Y<-X%*%beta+rnorm(N,sd=sd.w)
  beta.hat[,r]<-solve(t(X)%*%X)%*%t(X)%*%Y
  B0<-c(B0, beta.hat[1,r])
  B1<-c(B1, beta.hat[2,r])
  B2<-c(B2, beta.hat[3,r])
  #Y.hat<-c(Y.hat,t(xhat)%*%beta.hat[,r])
}

print(paste("VAR B0:", var(B0)))
print(paste("VAR B1:", var(B1)))
print(paste("VAR B2:", var(B2)))
