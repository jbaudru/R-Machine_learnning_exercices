load("UV.NL.Rdata")
X <- cbind(df["x1"], df["x2"], df["x3"], df["x4"], df["x5"], df["x6"], df["x7"], df["x8"], df["x9"], df["x10"])
Y <- df["Y"]
splits <- c(-1.0, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4 ,-0.3, -0.2, -0.1 , 0.0 , 0.1 , 0.2,  0.3, 0.4 , 0.5 , 0.6,  0.7,  0.8 , 0.9  ,1.0)

X
Y

# Split on regression tree
splitRT<-function(X,Y, splits){
  n<-NCOL(X)
  S<-length(splits)
  SSE<-numeric(n)
  bests<-numeric(n)
  for (f in 1:n){
    SSEs<-numeric(S)
    for (s in 1:S){
      I1<-which(X[,f]<splits[s])
      I2<-which(X[,f]>=splits[s])
      SSE1=sum((Y[I1,]-mean(Y[I1,])^2))
      SSE2=sum((Y[I2,]-mean(Y[I2,])^2))
      SSEs[s]<-SSE1+SSE2
    }
    SSE[f]=min(SSEs)
    bests[f]<-which.min(SSEs)
  }
  list(bestf=which.min(SSE),bestsplit=splits[bests[which.min(SSE)]])
}

splitRT(X, Y, splits)
