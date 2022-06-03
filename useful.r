# Maximise the likehood
for(w1 in W1){
    likelihood <- 1
    for(data in DN){
        likelihood <- likelihood * (w1*(dnorm(data,m1,s21)) + (1-w1)*(dnorm(data,m2,s22)))
    }
    if(likelihood > lik){
        lik <- likelihood
        max <- w1
    }
}

# Error Sum of Squares
sse <- function(b0, b1, x, y) {
  return(sum((y-b0-(b1*x))^2))
}

# Empirical risk
MISE_emp <- function(sse, N){
  return(sse/N)
}

MISE_emp <- function(X, Y, betahat){
  err=Y-X%*%betahat
  return (mean(err^2)/var(Y))
}

MISE_emp <- function(y, yhat){
  return (mean(y-yhat)^2)
}

MISE_loo <- function(Xtr, Ytr, K){ # K = Number of neighboors for KNNL
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Eloo1<-c(Eloo1,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K))
  }
  return(mean(Eloo1^2))
}

MISE_loo <- function(Xtr, Ytr, K){ # K = Number of hidden node Tree
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Eloo1<-c(Eloo1,Ytr[i]-treeL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K))
  }
  return(mean(Eloo1^2))
}

# Bais
biais_model <- function(yhat, y){
  return(mean(mean(yhat)-y))
}

# Var
var_model <- function(yhat){
  return (var(yhat))
}

# Correlation between X and Y
corXY=c(corXY,abs(cor(X[,j],Y)))

# FPE
FPE<-function(M,Y){
   beta=solve(t(M)%*%M)%*%t(M)%*%Y
   p=NCOL(M)
   Yhat=M%*%beta
   e=(Y-Yhat)
   N=length(Y)
   loo=numeric(N)
   for (i in 1:N){
      Mi=M[-i,]
      Yi=Y[-i]
      betai=solve(t(Mi)%*%Mi)%*%t(Mi)%*%Yi
      Yhati=M[i,]%*%betai
      loo[i]=Y[i]-Yhati
   }
   return(list(Fpe=(1+p/N)/(1-p/N)*mean(e^2),Remp=mean(e^2),beta0=beta[1],LOO=mean(loo^2)))
}

# KNNL
knnL<-function(Xtr,Ytr,Xts,K=1){
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

# Tree
treeL<-function(Xtr,Ytr,Xts,s=1){
  Nts=NROW(Xts)
  N=NROW(Xtr)
  n=NCOL(Xtr)
  df<-data.frame(cbind(Ytr,Xtr))
  colnames(df)[1]<-'Y'
  dfts<-data.frame(Xts)
  colnames(dfts)[1:n]<-colnames(df)[2:(n+1)]
  model<- tree(Y~.,df,minsize=s) # model = h
  Yhat=predict(model,dfts)
  return (Yhat)
}

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


# Find optimal lambda value
lambdaopti <- 0
Eloo2=NULL
for(i in 1:n){
  Xi=Xtr[-i,]
  Yi=Ytr[-i]
  Ytsi=Ytr[i]
  Xtsi=Xtr[i,]
  Xi1=Xi[,1]
  for(j in 1:length(lambda)){
    br=solve(t(Xi1)%*%Xi1 + lambda[j]*diag(1)) %*%t(Xi1)%*%Yi
    if(br<brmin){
      lambdaopti = lambda[j]
    }
  }
  Eloo2=c(Eloo2,Ytsi-(br[1]+ br[2]*Xtsi[2]))
}

# Select tje nmax most correlated feature to Y
fselected<-NULL
nmax=5
for (f in 1:nmax){
  MSEloo=numeric(n)+Inf
  for (j in setdiff(1:n,fselected)){
    subs<-c(fselected,j)
    eloo=numeric(N)
    for (i in 1:N){
      Xi=cbind(numeric(N-1)+1,X[-i,subs])
      Yi=Y[-i]
      betai=solve(t(Xi)%*%Xi)%*%t(Xi)%*%Yi
      yhati=c(1,X[i,subs])%*%betai
      eloo[i]=Y[i]-yhati
    }
    MSEloo[j]=mean(eloo^2)
  }
  fselected=c(fselected,which.min(MSEloo))
}
fsel=fselected

# Give the best split of the dataset (Like decision tree)
splits=seq(-2,2,by=0.5)
plitRT<-function(X,Y, splits){
  n<-NCOL(X)
  S<-length(splits)
  SSE<-numeric(n)
  bests<-numeric(n)
  for (f in 1:n){
    SSEs<-numeric(S)
    for (s in 1:S){
      I1<-which(X[,f]<splits[s])
      I2<-which(X[,f]>=splits[s])
      SSE1=sum((Y[I1]-mean(Y[I1])^2))
      SSE2=sum((Y[I2]-mean(Y[I2])^2))
      SSEs[s]<-SSE1+SSE2
    }
    SSE[f]=min(SSEs)
    bests[f]<-which.min(SSEs)
  }
  list(bestf=which.min(SSE),bestsplit=splits[bests[which.min(SSE)]])
}


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

plot(FPR1,TPR1,type="l",col="yellow",xlab="FPR",ylab="TPR",main="ROC curves")

# NAIVE BAYES CLASSIFIER FOR BINARY CLASS
TP=0
FP=0
TN=0
FN=0
for (i in 1:NROW(X)){
  x=X[i,]
  y=Y[i]
  NBp= dest(x[1],X[Ip,1])*dest(x[2],X[Ip,2])*Pp
  NBn= dest(x[1],X[In,1])*dest(x[2],X[In,2])*Pn
  # CONFUSION MATRIX
  if (NBp>NBn){
    if (y>0)
      TP=TP+1
    else
      FP=FP+1
  }
  if (NBp<NBn){
    if (y<0)
      TN=TN+1
    else
      FN=FN+1
  }
}
cat("TP=",TP,"| FP=",FP,"| TN=",TN,"| FN=",FN)

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
ASSC=assess(Y,Yhat)
TP = ASSC$TP
