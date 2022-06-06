py = c(0.6, 0.5, 0.99, 0.49, 0.1, 0.26, 0.33, 0.15, 0.05)
y = c(1, -1, 1, -1, -1, -1, 1, -1, -1)
data = cbind(py, y)
newdata <- data[order(py),]

assess<-function(Y,Yhat){
  if (length(Y)!=length(Yhat))
    stop("wrong sizes")
  TP=length(which(Y==1 & Yhat==1))
  FP=length(which(Y==-1 & Yhat==1))
  TN=length(which(Y==-1 & Yhat==-1))
  FN=length(which(Y==1 & Yhat==-1))
  P=length(which(Y==1))
  N=length(which(Y==-1))
  SE = 1-(FN/P)
  Phat=length(which(Yhat==1))
  Nhat=length(which(Yhat==-1))
  list(TP=TP,FP=FP,FN=FN,TN=TN,TPR=TP/P, FPR=FP/N, TNR=TN/N, PR=TP/Phat,ER=(FP+FN)/(P+N),BER=0.5*(FP/(TN+FP)+FN/(FN+TP)),SE=SE)
}


FPR = NULL
SE = NULL
for (j in 1:9){
  findata = NULL
  yhat = NULL
  for (i in 1:9){
    if(newdata[i,1]>newdata[j,1]){
      yhat = cbind(yhat, 1)
    }
    else{
      yhat = cbind(yhat, -1)
    }
  }
  findata <- cbind(newdata, t(yhat))
  ASSC=assess(findata[,2],findata[,3])
  se = ASSC$SE
  fpr = ASSC$FPR
  SE = cbind(SE, se)
  FPR = cbind(FPR, fpr)
}
png(file="Q16.png")
plot(FPR, SE, xlab="FPR", ylab="SE", col="red", lwd=2, main="AUC")
lines(FPR, SE, pch=16)
