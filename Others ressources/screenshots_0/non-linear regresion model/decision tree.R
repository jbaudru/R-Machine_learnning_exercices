rm(list=ls())

load("UV.NL.Rdata")



Y=df$Y

X=df[,-1]

N=length(Y)

n=NCOL(X)

Splits<-seq(-1,1,by=0.1)

S<-length(Splits)



Delta<-array(0,c(n,S))

for (i in 1:n){

  for (s in 1:S){

    I1=which(X[,i]<=Splits[s])

    I2=which(X[,i]>Splits[s])

    muhat1=mean(Y[I1])

    muhat2=mean(Y[I2])

    Delta[i,s]=sum((Y[I1]-muhat1)^2)+sum((Y[I2]-muhat2)^2)

    

  }

}

w=which(Delta==min(Delta), arr.ind=T)

cat("Split at", Splits[w[2]], " on var ", w[1],"\n")
