X = cbind(numeric(7)+1, c(0.1, 0, -0.3, 0.3, 0.4, 0.1, -1))
Y = c(1, 0.5, 1.2, 1, 0.5, 0, 1.1)

# FOR KNN
MISE_loo <- function(Xtr, Ytr, K){ # K = Number of neighboors
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Yhat = knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K)
    Eloo1<-c(Eloo1,Ytr[i]-Yhat)
  }
  return(mean(Eloo1^2))
}

knnL<-function(Xtr,Ytr,Xts,K=1){
  Nts=NROW(Xts)
  N=NROW(Xtr)
  Yhat=numeric(Nts)
  for (i in 1:Nts){
    Distance=apply((Xtr-array(1,c(N,1))%*%Xts[i,])^2,1,mean)
    iD=sort(Distance, decreasing=FALSE, index=TRUE)$ix[1:K]
    Yhat[i]=mean(Ytr[iD])
  }
  return(Yhat)
}

print(paste("Training 1 MISEloo:", MISE_loo(X, Y, 1)))
