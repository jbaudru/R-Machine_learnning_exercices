load("EXAM.Rdata")
D=Q5.G1.D

Ip=which(D[,3]==1) # Index Y == 1
In=which(D[,3]==-1) # Index Y == -1

classes=c("-1","1")
N=NROW(D)
n=2
X=D[,1:n] # Inputs
Y=D[,n+1] # Output

plot(X[Ip,1],X[Ip,2],col="green")
points(X[In,1],X[In,2],col="red")

Pp=length(Ip)/N # p(y=1)
Pn=1-Pp # p(y=-1)

# For the Normal dist
Sigmap=var(X[Ip,])
Sigman=var(X[In,])
mup=apply(X[Ip,],2,mean)
mun=apply(X[In,],2,mean)

dest<-function(x,X){
  return (dnorm(x,mean(X),sd(X)))
}

TP=0
FP=0
TN=0
FN=0
# NAIVE BAYES CLASSIFIER FOR BINARY CLASS
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
