rm(list=ls())

load("UV.NL.Rdata")



norm<-function(x){

  sum(x^2)

}

M=4

mu=rbind(c(-1,-1),c(-1,1), c(1,-1),c(1,1))

sigma=1





Ytr=df2tr$Y

Xtr=df2tr[,-1]

Yts=df2ts$Y

Xts=df2ts[,-1]

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

etr=Ytr-rXtr%*%betahat

cat(mean(etr^2)/var(Ytr))

ets=Yts-rXts%*%betahat

cat(mean(ets^2)/var(Yts))
