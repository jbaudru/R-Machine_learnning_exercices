CLASSES=c("-1","1")
## a priori probabilities des classes
X = c(0, -1, 1)
for (i in 1:length(X)){
  x = X[i]
  p.p=0.5 # p(y=1)
  p.m=1-p.p

  ## p(x| y=1)
  px.p=dnorm(x,-1,1)

  ## p(x| y=-1)
  px.m=dnorm(x,1,1)

  ## conditional probabilities (BAYES THEOREM)
  # p(y=1|x)
  py.p=px.p*p.p/(px.p*p.p+px.m*p.m)
  # p(y=-1|x)
  py.m=px.m*p.m/(px.p*p.p+px.m*p.m)
  print(x)
  print(paste("   P(y=1|x)=",py.p))
  print(paste("   P(y=-1|x)=",py.m))
}
