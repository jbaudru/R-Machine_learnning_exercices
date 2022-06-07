rm(list=ls())
load("EXAM.1s.22.Rdata")
set.seed(0)

# For binary classification
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

newdata = Q4.G1.D
N=NROW(newdata)


# CLASSIFIER 1 Nayes Bayes Normal condi density
#mz = mean(newdata[,3])+((sd(newdata[,3])/sd(newdata[,1]))* cov(newdata[,1], mean(newdata[,1])))
#sz = (1-(cov(newdata[,1], mean(newdata[,1])))^2)*var(newdata[,3])
#z = rnorm(mz, sz)

FPR = NULL
TPR = NULL
# CLASSIFIER 2
for (j in 1:N){
  yhat = NULL
  for (i in 1:N){
    if(abs(newdata[i,1])>newdata[j,1]){
      yhat = cbind(yhat, 1)
    }
    else{
      yhat = cbind(yhat, -1)
    }
  }
  findata <- cbind(newdata, t(yhat))
  ASSC=assess(findata[,3],findata[,4])
  tpr = ASSC$TPR
  fpr = ASSC$FPR
  TPR = cbind(TPR, tpr)
  FPR = cbind(FPR, fpr)
}
plot(FPR, TPR, xlab="FPR", ylab="TPR", col="red", lwd=2, main="ROC Classifier 2")
