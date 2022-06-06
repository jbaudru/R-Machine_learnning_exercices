library(MASS)

n<-2 # number input variables
p<-n+1
N=12
X<-cbind(c(-1,-0.5,0.5,1,-1,-0.5,0.5,1,-1,-0.5,0.5,1 ),c(-1,-1,-1,-1,0,0,0,0,1,1,1,1 ))
X<-cbind(array(1,c(N,1)),X)
beta<-c(1,0.2,-0.1)
B<-10000
sd.w<-1
Y<-c(3.7819313, 2.8513878, 2.0059245,2.1208149,-0.3334663,1.4889079,3.1077047,1.9725575,2.1113995,0.6039862,2.3499093,1.8995413)

xhat=array(c(1,1,1,1),c(p,1))
beta.hat<-array(0,c(p,B))

Y.hat<-NULL
for (b in 1:B){
  Ib<-sample(1:N,N,replace=TRUE)
  Xb<-X[Ib,]
  Yb<-Y[Ib] #X%*%beta+rnorm(N,sd=sd.w)
  beta.hat[,b]<-solve(t(Xb)%*%Xb)%*%t(Xb)%*%Yb
  Y.hat<-c(Y.hat,t(xhat)%*%beta.hat[,b])
}

# comparison analytical and simulated variance of the prediction
print(paste("Prediction Var[y]:", sd.w^2*(t(xhat)%*%ginv(t(X)%*%X)%*%xhat)))
print(paste("Bootstrap estimate Var[y]:", var(Y.hat)))
