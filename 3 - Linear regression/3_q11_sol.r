load("EXAM.Rdata")

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

D=Q4.G1.D
X=D[,1:2]
Y=D[,3]
N=NROW(D)
n=NCOL(X)

onesN=numeric(N)+1
M1=array(onesN,c(N,1))

M2=cbind(onesN,X[,1],X[,2])
M3=cbind(onesN, X[,1])
M4=cbind(onesN, X[,1]^2,X[,2]^2)
M5=cbind(onesN, X[,1],X[,2], X[,1]^2,X[,2]^2,X[,1]*X[,2], X[,1]^3,X[,2]^3)
M6=cbind(onesN, X[,1]^2)
M7=cbind(onesN, X[,2]^2)
M8=cbind(onesN, X[,1]^3)
M9=cbind(onesN, X[,1]^2,X[,2]^2,X[,1]*X[,2])

FP=NULL
Remp=NULL
beta0=NULL
LOO=NULL
for (i in 1:9){
   eval(parse(text=paste("FP=c(FP,FPE(M",i,",Y)$Fpe)",sep="")))
   eval(parse(text=paste("LOO=c(LOO,FPE(M",i,",Y)$LOO)",sep="")))
   eval(parse(text=paste("Remp=c(Remp,FPE(M",i,",Y)$Remp)",sep="")))
   eval(parse(text=paste("beta0=c(beta0,FPE(M",i,",Y)$beta0)",sep="")))
}

cat("which.min(Remp)=",which.min(Remp),
    "\nwhich.min(FP)=",which.min(FP),
    "\nbeta0_1=",beta0[1],
    "\nbeta0_9=",beta0[9],"\n" )

cat("Remp=",Remp,"\n")
cat("FP=",FP,"\n")
cat("LOO=",LOO,"\n -- \n")
