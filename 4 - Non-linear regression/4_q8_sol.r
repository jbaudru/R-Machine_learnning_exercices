# 2 inputs & 1 output
# By using R, fit an RBF with M=4 basis functions with equation
# ρm(x)=exp(−||x−μm||^2/σ2) where
# μ1=[1,1]T
# μ2=[1,−1]T
# μ3=[−1,1]T
# μ4=[−1,−1]T
# and σ2=1
load(".Rdata")

MISE <- function(X, Y, betahat){
  err=Y-X%*%betahat
  return (mean(err^2)/var(Y))
}

norm<-function(x){
  sum(x^2)
}

M=4
mu=rbind(c(-1,-1),c(-1,1), c(1,-1),c(1,1))
sigma=1
Ytr=df2tr$Y
Xtr=df2tr[,-1] # X1 & X2
Yts=df2ts$Y
Xts=df2ts[,-1] # X1 & X2
Ntr=length(Ytr)
Nts=length(Yts)
rXtr=array(0,c(NROW(Xtr),M))
rXts=array(0,c(NROW(Xts),M))
for (i in 1:Ntr){
  for (m in 1:M)
    rXtr[i,m]=exp(-norm(Xtr[i,]-mu[m,])/sigma^2)
}

for (i in 1:Nts){
  for (m in 1:M)
    rXts[i,m]=exp(-norm(Xts[i,]-mu[m,])/sigma^2)
}

betahat=solve(t(rXtr)%*%rXtr)%*%t(rXtr)%*%Ytr

print(paste("Training MISEemp:", MISE(rXtr, Ytr, betahat)))
print(paste("Testing MISEemp:", MISE(rXts, Yts, betahat)))
