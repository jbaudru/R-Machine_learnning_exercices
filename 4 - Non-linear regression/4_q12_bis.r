load("EXAM.Rdata")

# FOR KNN & Tree
MISE_emp <- function(y, yhat){
  return (mean((y-yhat)^2))
}

# FOR KNN
MISE_loo <- function(Xtr, Ytr, K){ # K = Number of neighboors
  Ntr=length(Ytr)
  Eloo1<-NULL
  for (i in 1:Ntr){
    Eloo1<-c(Eloo1,Ytr[i]-knnL(Xtr[-i,],Ytr[-i],t(Xtr[i,]),K))
  }
  return(mean(Eloo1^2))
}

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

x1 = Q4.G1.D[,1]
x2 = Q4.G1.D[,2]
y = Q4.G1.D[,3]

Xtr=cbind(x1,x2)
Ytr=y

Yhat1=knnL(Xtr,Ytr,Xtr,1)
Yhat2=knnL(Xtr,Ytr,Xtr,2)
Yhat3=knnL(Xtr,Ytr,Xtr,3)
Yhat4=knnL(Xtr,Ytr,Xtr,4)
Yhat5=knnL(Xtr,Ytr,Xtr,5)
Yhat6=knnL(Xtr,Ytr,Xtr,6)
Yhat7=knnL(Xtr,Ytr,Xtr,7)

print(paste("Training 1 MISEemp:", MISE_emp(Ytr, Yhat1)))
print(paste("Training 2 MISEemp:", MISE_emp(Ytr, Yhat2)))
print(paste("Training 3 MISEemp:", MISE_emp(Ytr, Yhat3)))
print(paste("Training 4 MISEemp:", MISE_emp(Ytr, Yhat4)))
print(paste("Training 5 MISEemp:", MISE_emp(Ytr, Yhat5)))
print(paste("Training 6 MISEemp:", MISE_emp(Ytr, Yhat6)))
print(paste("Training 7 MISEemp:", MISE_emp(Ytr, Yhat7)))

print(paste("Training 1 MISEloo:", MISE_loo(Xtr, Ytr, 1)))
print(paste("Training 2 MISEloo:", MISE_loo(Xtr, Ytr, 2)))
print(paste("Training 3 MISEloo:", MISE_loo(Xtr, Ytr, 3)))
print(paste("Training 4 MISEloo:", MISE_loo(Xtr, Ytr, 4)))
print(paste("Training 5 MISEloo:", MISE_loo(Xtr, Ytr, 5)))
print(paste("Training 6 MISEloo:", MISE_loo(Xtr, Ytr, 6)))
print(paste("Training 7 MISEloo:", MISE_loo(Xtr, Ytr, 7)))


Nts=NROW(Xtr)

Xts=c(0,0)

Yhat1=knnL(Xtr,Ytr,Xtr,1)
Yhat2=knnL(Xtr,Ytr,Xtr,2)
Yhat3=knnL(Xtr,Ytr,Xtr,3)
Yhat4=knnL(Xtr,Ytr,Xtr,4)
Yhat5=knnL(Xtr,Ytr,Xtr,5)
Yhat6=knnL(Xtr,Ytr,Xtr,6)
Yhat7=knnL(Xtr,Ytr,Xtr,7)

print(paste("Model 1 Yhat:", mean(Yhat1)))
print(paste("Model 2 Yhat:", mean(Yhat2)))
print(paste("Model 3 Yhat:", mean(Yhat3)))
print(paste("Model 4 Yhat:", mean(Yhat4)))
print(paste("Model 5 Yhat:", mean(Yhat5)))
print(paste("Model 6 Yhat:", mean(Yhat6)))
print(paste("Model 7 Yhat:", mean(Yhat7)))
