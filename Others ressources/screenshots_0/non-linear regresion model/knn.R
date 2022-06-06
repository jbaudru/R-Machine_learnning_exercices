rm(list=ls())

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



Xtr=cbind(c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1,0.1,0.2,0.9),

          c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3,0.7,-0.5,0.3))

Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3,0.1, -0.2, 0.1)



Yhat1=knnL(Xtr,Ytr,Xtr,1) 
Yhat2=knnL(Xtr,Ytr,Xtr,3) 
Yhat3=knnL(Xtr,Ytr,Xtr,5) 

MISEemp1=mean((Ytr-Yhat1)^2)
MISEemp2=mean((Ytr-Yhat2)^2)
MISEemp3=mean((Ytr-Yhat3)^2)

Ntr=length(Ytr)

Eloo1<-NULL
Eloo2<-NULL
Eloo3<-NULL

for (i in 1:Ntr){
  Eloo1<-c(Eloo1,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),1))
  Eloo2<-c(Eloo2,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),3))
  Eloo3<-c(Eloo3,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),5))
}



MISEloo1=mean(Eloo1^2)
MISEloo2=mean(Eloo2^2)
MISEloo3=mean(Eloo3^2)
