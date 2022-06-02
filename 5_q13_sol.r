load("FS2_1.Rdata")

MISE_emp <- function(y, yhat){
  return (mean(y-yhat)^2)
}

n=NCOL(X)
N=NROW(X)

fselected<-4

Xhat=scale(X)
S=svd(Xhat)
Z=Xhat%*%S$v

Xhats=scale(Xts)
Zts=Xhats%*%S$v

X=cbind(numeric(N)+1,X)
Xts=cbind(numeric(NROW(Xts))+1,Xts)

betahat=solve(t(X)%*%X)%*%t(X)%*%Y
Yhats=Xts%*%betahat
print(paste("Test all features MISEemp:", MISE_emp(Yts, Yhats)))

# features selection
Z=Z[,1:fselected]
Zts=Zts[,1:fselected]

Z=cbind(numeric(N)+1,Z)
Zts=cbind(numeric(NROW(Zts))+1,Zts)
betahat=solve(t(Z)%*%Z)%*%t(Z)%*%Y
Yhats2=Zts%*%betahat
print(paste("Test cor. features MISEemp:", MISE_emp(Yts, Yhats2)))
