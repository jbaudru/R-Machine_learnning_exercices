load("FS6.Rdata")
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
      SSE1=sum((Y[I1]-mean(Y[I1])^2))
      SSE2=sum((Y[I2]-mean(Y[I2])^2))
      SSEs[s]<-SSE1+SSE2
    }
    SSE[f]=min(SSEs)
    bests[f]<-which.min(SSEs)
  }
  list(bestf=which.min(SSE),bestsplit=splits[bests[which.min(SSE)]])
}

n=NCOL(X)
N=NROW(X)
splits=seq(-2,2,by=0.5)

Spl1<-splitRT(X,Y,splits)
fs1<-Spl1$bestf
print(paste("Best features:",fs1))

I1<-which(X[,fs1]<Spl1$bestsplit)
I2<-which(X[,fs1]>=Spl1$bestsplit)

Spl2<-splitRT(X[I1,],Y[I1],splits)
fs2<-Spl2$bestf
print(paste("Best features:",fs2))

Spl3<-splitRT(X[I2,],Y[I2],splits)
fs3<-Spl3$bestf
print(paste("Best features:",fs3))
