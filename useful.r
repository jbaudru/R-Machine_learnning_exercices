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
