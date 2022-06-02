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

Y.hat<-NULL
for (r in 1:R){
  Y<-X%*%beta+rnorm(N,sd=sd.w)
  beta.hat[,r]<-solve(t(X)%*%X)%*%t(X)%*%Y
  Y.hat<-c(Y.hat,t(xhat)%*%beta.hat[,r])
}

# comparison analytical and simulated variance of the prediction
print(paste("Prediction variance:", sd.w^2*(t(xhat)%*%ginv(t(X)%*%X)%*%xhat)))
print(paste("MC value:", var(Y.hat)))
