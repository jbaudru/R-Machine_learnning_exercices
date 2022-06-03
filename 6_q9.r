#install.packages("mvtnorm")
library("mvtnorm")

X=rbind(c(-2,-2),c(0,-2),c(2,0),c(0,1),c(1,1),c(3,3))

for (i in 1:NROW(X)){
  x=X[i,]
  CLASSES=c("-1","1")

  # a priori probabilities
  S=cbind(c(2,1),c(1,2))

  p.p=0.75
  p.m=1-p.p
  w1=0.5
  w2=0.5

  # inverse proababilities
  px.p=dmvnorm(x,mean=c(-1,-1),sigma=S)

  # p (x=0| y=1)
  px.m=w1*dmvnorm(x,mean=(c(1,1)),sigma=S)+w2*dmvnorm(x,mean=(c(0,0)),sigma=S)

  ## conditional probabilities
  # p (y=1|x)
  py.p=px.p*p.p/(px.p*p.p+px.m*p.m)
  # p (y=-1|x)
  py.m=px.m*p.m/(px.p*p.p+px.m*p.m)
  print(paste("If x=",x, ":"))
  print(paste("   P(y=1|x)=",py.p))
  print(paste("   P(y=-1|x)=",py.m))
  print(paste("   Class=", CLASSES[which.max(c(py.m,py.p))]))
}
