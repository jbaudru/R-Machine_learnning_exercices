load("FS4.Rdata")
n=NCOL(X)
N=NROW(X)

MISE_emp <- function(y, yhat){
  return (mean((y-yhat)^2))
}

corXY=NULL
for (j in 1:n){
  corXY=c(corXY,abs(cor(X[,j],Y)))
}

fsel=sort(corXY,decre=TRUE,index=TRUE)$ix[1:5]
X2=X[,fsel]
Xts2=Xts[,fsel]

X=cbind(numeric(N)+1,X)
Xts=cbind(numeric(NROW(Xts))+1,Xts)
betahat=solve(t(X)%*%X)%*%t(X)%*%Y
Yhats=Xts%*%betahat
print(paste("Test all features MISEemp:", MISE_emp(Yts, Yhats)))


X2=cbind(numeric(N)+1,X2)
Xts2=cbind(numeric(NROW(Xts))+1,Xts2)
betahat2=solve(t(X2)%*%X2)%*%t(X2)%*%Y
Yhats2=Xts2%*%betahat2
print(paste("Test cor. features MISEemp:", MISE_emp(Yts, Yhats2)))
