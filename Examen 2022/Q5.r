rm(list=ls())
load("EXAM.1s.22.Rdata")
set.seed(0)

X <- Q2.G1.D[,1]
Y <- Q2.G1.D[,2]

N=100

# As argmax log Lh = argmax Lh
lik <- -1
maxtheta <- 0
LIK = NULL
for(theta in Theta){
  Yhat=NULL
  Yhat2=NULL
  likelihood <- 1
  for (x in X){
    likelihood <- likelihood * (sin(theta*x)+dnorm(x,0,1))
  }
  LIK <- cbind(LIK, log(likelihood))
  if(likelihood > lik){
      lik <- likelihood
      maxtheta <- theta
  }
}
print(paste("theta which maximize :",maxtheta))

YH = NULL
for(x in X){
  Yhat = sin(-0.5*x)+dnorm(x,0,1)
  YH = cbind(YH, Yhat)
}

pdf(file="linear_regression.pdf")
plot(X, Y, xlab ="x", ylab ="y", col ="black", main="Linear regression")
data <-cbind(X, YH[1,])
newdata <- data[order(X),]
lines(newdata[,1], newdata[,2], col="red", lwd=2)

pdf(file="likehood_evolution.pdf")
plot(Theta, LIK, xlab ="Theta", ylab ="Likehood", col ="black", main="Evolution of Likehood")
lines(Theta, LIK, col="red", lwd=2)
