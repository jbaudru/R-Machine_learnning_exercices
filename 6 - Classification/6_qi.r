load("EXAM.Rdata")

# NAIVE BAYES CLASSIFIER
NB<-function(X,Y){
  N=length(Y)
  n=NCOL(X)
  Yhat=numeric(N)
  I1=which(Y==1)
  I0=which(Y==-1)
  p1=length(I1)/N
  p0=1-p1
  for (i in 1:N){
    p1x=1
    p0x=1
    for (j in 1:n){
      p1x=p1x*dnorm(X[i,j],mean(X[I1,j]),sd(X[I1,j]))
      p0x=p0x*dnorm(X[i,j],mean(X[I0,j]),sd(X[I0,j]))
    }
    Yhat[i]=p1x*p1/(p1x*p1+p0x*p0)
  }
  return(Yhat)
}

D=Q6.G1.D
N= nrow(D)
n=2

X=D[,1:n] #
Y=D[,n+1]

N1=length(which(Y==1)) # Nb line where Y == 1
N0=length(which(Y==-1)) # Nb line where Y == -1

Yhat1=X[,1]
Yhat2=NB(X,Y)

s1<-sort(Yhat1,decreasing=FALSE,index=TRUE)
s2<-sort(Yhat2,decreasing=FALSE,index=TRUE)

# ROC CURVE COMPUTATION
TPR1=NULL
FPR1=NULL
TPR2=NULL
FPR2=NULL
for (i in 1:N){
  I1=s1$ix[1:i]
  TPR1=c(TPR1,length(which(Y[setdiff(1:N,I1)]==1))/N1)
  FPR1=c(FPR1,length(which(Y[setdiff(1:N,I1)]==-1))/N0)

  I2=s2$ix[1:i]
  TPR2=c(TPR2,length(which(Y[setdiff(1:N,I2)]==1))/N1)
  FPR2=c(FPR2,length(which(Y[setdiff(1:N,I2)]==-1))/N0)
}

png(file="Qi.png")
plot(FPR1,TPR1,type="l",col="red",xlab="FPR",ylab="TPR",main="ROC curves")
lines(FPR1,FPR1,lty=2)
lines(FPR2,TPR2,col="black")
legend(0.5,0.3,c("TH","NB"),col=c("red","black"),lty=1,text.font = 2,pt.cex = 1, cex = 0.7,ncol=3)
