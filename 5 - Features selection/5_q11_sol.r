load("FS5.Rdata")
n=NCOL(X)
N=NROW(X)

MISE_emp <- function(y, yhat){
  return (mean(y-yhat)^2)
}

fselected<-NULL
nmax=5
for (f in 1:nmax){
  MSEloo=numeric(n)+Inf
  for (j in setdiff(1:n,fselected)){
    subs<-c(fselected,j)
    eloo=numeric(N)
    for (i in 1:N){
      Xi=cbind(numeric(N-1)+1,X[-i,subs])
      Yi=Y[-i]
      betai=solve(t(Xi)%*%Xi)%*%t(Xi)%*%Yi
      yhati=c(1,X[i,subs])%*%betai
      eloo[i]=Y[i]-yhati
    }
    MSEloo[j]=mean(eloo^2)
  }
  fselected=c(fselected,which.min(MSEloo))
}
fsel=fselected

X=cbind(numeric(N)+1,X)
Xts=cbind(numeric(NROW(Xts))+1,Xts)
betahat=solve(t(X)%*%X)%*%t(X)%*%Y
Yhats=Xts%*%betahat
print(paste("Test all features MISEemp:", MISE_emp(Yts, Yhats)))

# features selection
X2=X[,fsel]
Xts2=Xts[,fsel]

X2=cbind(numeric(N)+1,X2)
Xts2=cbind(numeric(NROW(Xts))+1,Xts2)
betahat2=solve(t(X2)%*%X2)%*%t(X2)%*%Y
Yhats2=Xts2%*%betahat2
print(paste("Test cor. features MISEemp:", MISE_emp(Yts, Yhats2)))
