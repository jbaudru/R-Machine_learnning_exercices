rm(list=ls())
load("EXAM.1s.22.Rdata")
set.seed(0)

X = Q3.G2.D[,1:10] # first 10 columns contain the inputs
Y = Q3.G2.D[,ncol(Q3.G2.D)] # the last column contains the output

n=NCOL(X)
N=NROW(X)
X<-cbind(numeric(N)+1,X)

LAM=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
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
print(paste("Opti. lambda:", bestlam))

fselected<-NULL
nmax=5
for (f in 1:nmax){
  MSEloo=numeric(n)+Inf
  for (j in setdiff(1:n,fselected)){
    subs<-c(fselected,j)
    eloo=numeric(N)
    for (i in 1:N){
      Xi=cbind(numeric(N-1)+1,X[-i,subs])
      n=NCOL(Xi)
      Yi=Y[-i]
      betai= solve(t(Xi)%*%Xi+0.3*diag(n))%*%t(Xi)%*%Yi
      yhati=c(1,X[i,subs])%*%betai
      eloo[i]=Y[i]-yhati
    }
    MSEloo[j]=mean(eloo^2)
  }
  fselected=c(fselected,which.min(MSEloo))
}
print(paste("Selected features:",fselected[1:nmax]))
