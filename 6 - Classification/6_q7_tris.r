for (x in c(-2,-1,0.001,0.1,2,3)){
  CLASSES=c("-1","1")
  ## a priori probabilities
  p.p=0.5 # p(y=1)
  p.m=1-p.p

  ## inverse proababilities
  px.p=dnorm(x,-1,1)

  ## p(x=0| y=1)
  px.m=dnorm(x,1,1)

  ## conditional probabilities
  # p(y=1|x)
  py.p=px.p*p.p/(px.p*p.p+px.m*p.m)
  # p(y=-1|x)
  py.m=px.m*p.m/(px.p*p.p+px.m*p.m)

  print(paste("If x=",x, ":"))
  print(paste("   P(y=1|x)=",py.p))
  print(paste("   P(y=-1|x)=",py.m))
  print(paste("   Class=", CLASSES[which.max(c(py.m,py.p))]))
}
