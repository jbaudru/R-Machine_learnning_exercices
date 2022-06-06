rm(list=ls())



nnetL<-function(Xtr,Ytr,Xts,s=1){

  Nts=NROW(Xts)

  N=NROW(Xtr)

  n=NCOL(Xtr)

  df<-data.frame(cbind(Ytr,Xtr))

  colnames(df)[1]<-'Y'

  

  dfts<-data.frame(Xts)

  colnames(dfts)[1:n]<-colnames(df)[2:(n+1)]

  

  set.seed(0)

  h<- nnet(Y~.,df,size=s,rang=0.1,

           linout=TRUE,trace=FALSE)

  

  Yhat=predict(h,dfts)

  Yhat

}



Xtr=cbind(c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1,0.1,0.2,0.9),

          c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3,0.7,-0.5,0.3))

Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3,0.1, -0.2, 0.1)









Yhat1=nnetL(Xtr,Ytr,Xtr,1) 

Yhat2=nnetL(Xtr,Ytr,Xtr,3) 

Yhat3=nnetL(Xtr,Ytr,Xtr,5) 



MISEemp1=mean((Ytr-Yhat1)^2)

MISEemp2=mean((Ytr-Yhat2)^2)

MISEemp3=mean((Ytr-Yhat3)^2)



Ntr=length(Ytr)



Eloo1<-NULL

Eloo2<-NULL

Eloo3<-NULL

for (i in 1:Ntr){

  Eloo1<-c(Eloo1,Ytr[i]-nnetL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),1))

  Eloo2<-c(Eloo2,Ytr[i]-nnetL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),3))

  Eloo3<-c(Eloo3,Ytr[i]-nnetL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),5))

}





MISEloo1=mean(Eloo1^2)

MISEloo2=mean(Eloo2^2)

MISEloo3=mean(Eloo3^2)
