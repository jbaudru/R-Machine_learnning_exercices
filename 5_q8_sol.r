load("FS2.Rdata")

n=NCOL(X)
N=NROW(X)
X<-cbind(numeric(N)+1,X)

LAM=c(0,1,10,100,1000,10000,100000)
MSEloo=numeric(length(LAM))
for (l  in 1:length(LAM)){
  Eloo<-NULL
  lam=LAM[l]
  for (i in 1:N){
    Xi=X[-i,]
    Yi=Y[-i]
    betahat= solve(t(Xi)%*%Xi+lam*diag(n+1))%*%t(Xi)%*%Yi
    Eloo<-c(Eloo,Y[i]-X[i,]%*%betahat)
  }
  MSEloo[l]=mean(Eloo^2)
}
bestlam=LAM[which.min(MSEloo)]
print(paste("Optimal lambda:", bestlam))
