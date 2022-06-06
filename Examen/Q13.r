X = cbind(numeric(7)+1, c(0.5, 1, -1, -0.25, 0, 0.1, 0.25))

Xts = cbind(numeric(7)+1, c(seq(from = -2, to = 1, by = 0.5)))
Y = c(1, 1, 1, 1, 0.5, 0, 0.5)
K = 3

knnL<-function(Xtr,Ytr,Xts,K){
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

YH = NULL
for(i in 1:7){
  Yhat = knnL(X,Y,t(Xts[i, ]), K)
  #Yhat = (betahat[1]+ betahat[2]*X[i,2])
  YH = cbind(YH, Yhat)
}

print(YH)
png(file="Q13.png")
plot(X[,2], Y, xlab ="x", ylab ="y", col ="black", main="Linear regression")
lines(X[,2], YH, col="red", type="s")
