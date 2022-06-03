library(tree)
set.seed(0)

load("FS6.Rdata")
Xtr <- X
Ytr <- Y

treeL<-function(Xtr,Ytr,Xts,s=1){
  Nts=NROW(Xts)
  N=NROW(Xtr)
  n=NCOL(Xtr)
  df<-data.frame(cbind(Ytr,Xtr))
  colnames(df)[1]<-'Y'
  dfts<-data.frame(Xts)
  colnames(dfts)[1:n]<-colnames(df)[2:(n+1)]
  model<- tree(Y~.,df,minsize=s) # model = h
  print(model)
  Yhat=predict(model,dfts)
  return (Yhat)
}

Yhat1=treeL(Xtr,Ytr,Xts,2)
