library(nnet)
set.seed(0)

# FOR KNN & Tree
MISE_emp <- function(y, yhat){
  return (mean((y-yhat)^2))
}

# FOR KNN
MISE_loo <- function(Xtr, Ytr, K){ # K = Number of hidden node
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Eloo1<-c(Eloo1,Ytr[i]-nnetL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K))
  }
  return(mean(Eloo1^2))
}

nnetL<-function(Xtr,Ytr,Xts,s=1){
  Nts=NROW(Xts)
  N=NROW(Xtr)
  n=NCOL(Xtr)
  df<-data.frame(cbind(Ytr,Xtr))
  colnames(df)[1]<-'Y'
  dfts<-data.frame(Xts)
  colnames(dfts)[1:n]<-colnames(df)[2:(n+1)]
  model<-nnet(Y~.,df,size=s,rang=0.1, linout=TRUE,trace=FALSE)
  #model<- tree(Y~.,df,minsize=s) # model = h
  Yhat=predict(model,dfts)
  return (Yhat)
}

# X1 & X2 train
Xtr=cbind(c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1,0.1,0.2,0.9),c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3,0.7,-0.5,0.3))
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3,0.1, -0.2, 0.1)

Yhat1=nnetL(Xtr,Ytr,Xtr,1)
Yhat2=nnetL(Xtr,Ytr,Xtr,3)
Yhat3=nnetL(Xtr,Ytr,Xtr,5)

print(paste("Training 1 MISEemp:", MISE_emp(Ytr, Yhat1)))
print(paste("Training 2 MISEemp:", MISE_emp(Ytr, Yhat2)))
print(paste("Training 3 MISEemp:", MISE_emp(Ytr, Yhat3)))

print(paste("Training 1 MISEloo:", MISE_loo(Xtr, Ytr, 1)))
print(paste("Training 2 MISEloo:", MISE_loo(Xtr, Ytr, 3)))
print(paste("Training 3 MISEloo:", MISE_loo(Xtr, Ytr, 5)))
