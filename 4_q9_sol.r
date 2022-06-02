MISE_emp <- function(y, yhat){
  return (mean(y-yhat)^2)
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
  Yhat
}

# X1 & X2 train
Xtr=cbind(c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1),c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3))
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3)
# X1 & X2 test
Xts=cbind(c(1.6,-1.1,1.1,0.9,-0.1),c(0.5,-0.1,-0.2,0.3,-0.3))
Yts=c(-1.4, -0.3,  -1, 0.7,-0.3)

Yhat1=knnL(Xtr,Ytr,Xtr,1)
Yhat2=knnL(Xtr,Ytr,Xtr,3)
Yhat3=knnL(Xtr,Ytr,Xtr,5)

print(paste("Training 1 MISEemp:", MISE_emp(Ytr, Yhat1)))
print(paste("Training 2 MISEemp:", MISE_emp(Ytr, Yhat2)))
print(paste("Training 3 MISEemp:", MISE_emp(Ytr, Yhat3)))

Yhats1=knnL(Xtr,Ytr,Xts,1)
Yhats2=knnL(Xtr,Ytr,Xts,3)
Yhats3=knnL(Xtr,Ytr,Xts,5)

print(paste("Testing 1 MISEemp:", MISE_emp(Yts, Yhats1)))
print(paste("Testing 2 MISEemp:", MISE_emp(Yts, Yhats2)))
print(paste("Testing 3 MISEemp:", MISE_emp(Yts, Yhats3)))
