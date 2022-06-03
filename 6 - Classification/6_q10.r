load("EXAM.Rdata")
D=Q5.G1.D

Ip=which(D[,3]==1) # Index Y == 1
In=which(D[,3]==-1) # Index Y == -1

classes=c("-1","1")
N=NROW(D)
n=2
X=D[,1:n] # Inputs

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

Xts=rbind(c(0,0),c(1,1),c(-1,-1),c(2,2))

# NAIVE BAYES CLASSIFIER FOR BINARY CLASS
for (i in 1:NROW(Xts)){
  x=Xts[i,]
  NBp= dest(x[1],X[Ip,1])*dest(x[2],X[Ip,2])*Pp
  NBn= dest(x[1],X[In,1])*dest(x[2],X[In,2])*Pn
  cat("Prediction in x= [", x, "] is ",classes[which.max(c(NBn,NBp))],"\n")
}
