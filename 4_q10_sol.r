# FOR KNN & Tree
MISE_emp <- function(y, yhat){
  return (mean(y-yhat)^2)
}

# FOR KNN
MISE_loo <- function(Xtr, Ytr, K){ # K = Number of neighboors
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Eloo1<-c(Eloo1,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K))
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

Xtr=cbind(c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1,0.1,0.2,0.9),c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3,0.7,-0.5,0.3))
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3,0.1, -0.2, 0.1)

Yhat1=knnL(Xtr,Ytr,Xtr,1)
Yhat2=knnL(Xtr,Ytr,Xtr,3)
Yhat3=knnL(Xtr,Ytr,Xtr,5)

print(paste("Training 1 MISEemp:", MISE_emp(Ytr, Yhat1)))
print(paste("Training 2 MISEemp:", MISE_emp(Ytr, Yhat2)))
print(paste("Training 3 MISEemp:", MISE_emp(Ytr, Yhat3)))

print(paste("Training 1 MISEloo:", MISE_loo(Xtr, Ytr, 1)))
print(paste("Training 2 MISEloo:", MISE_loo(Xtr, Ytr, 3)))
print(paste("Training 3 MISEloo:", MISE_loo(Xtr, Ytr, 5)))
