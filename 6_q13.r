load("EXAM.Rdata")

# RETURN : TP, FP, FN, TN, TPR, FPR, TNR, PR, ER & BER
assess<-function(Y,Yhat){
  if (length(Y)!=length(Y))
    stop("wrong sizes")
  TP=length(which(Y==1 & Yhat==1))
  FP=length(which(Y==-1 & Yhat==1))
  TN=length(which(Y==-1 & Yhat==-1))
  FN=length(which(Y==1 & Yhat==-1))
  P=length(which(Y==1))
  N=length(which(Y==-1))
  Phat=length(which(Yhat==1))
  Nhat=length(which(Yhat==-1))
  list(TP=TP,FP=FP,FN=FN,TN=TN,TPR=TP/P, FPR=FP/N, TNR=TN/N, PR=TP/Phat,ER=(FP+FN)/(P+N),BER=0.5*(FP/(TN+FP)+FN/(FN+TP)))
}

D=Q5.G1.D

X=D[,1:2]
Y=D[,3]

N=NROW(X)
n=NCOL(X)

# CLASSIFIER 1
I1=which(X[,1]>0)
C1hat=numeric(N)-1
C1hat[I1]=1
# CLASSIFIER 2
I2=which(X[,1]*X[,2]<0)
C2hat=numeric(N)-1
C2hat[I2]=1
# CLASSIFIER 3
I3=which(X[,2]<0)
C3hat=numeric(N)-1
C3hat[I3]=1

PR=NULL
ER=NULL
BER=NULL
TPR=NULL
TNR=NULL
for (i in 1:3){
  eval(parse(text=paste("ASSC=assess(Y,C",i,"hat)",sep="")))
  TP=ASSC$TP
  FP=ASSC$FP
  FN=ASSC$FN
  TN=ASSC$TN
  ER<-c(ER,ASSC$ER)
  BER<-c(BER,ASSC$BER)
  PR=c(PR,ASSC$PR)
  TPR=c(TPR,ASSC$TPR)
  TNR=c(TNR,ASSC$TNR)
  cat("Classifier",i,":")
  print(rbind(c(TP,FP),c(FN,TN)))
}

cat("\nWhich.min(ER) =",which.min(ER),
    "\nWhich.min(BER) =",which.min(BER),
    "\nWhich.max(PR) =",which.max(PR),
    "\nWhich.max(TPR) =",which.max(TPR),
    "\nWhich.max(TNR) =",which.max(TNR),
    "TNR=", TNR,"\n")
