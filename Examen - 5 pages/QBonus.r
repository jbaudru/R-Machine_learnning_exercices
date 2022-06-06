X = cbind(c(-0.2, 0.1, 1, 0.1, -0.4, 0.1, 1), c(0.1, 0, -0.3, 0.2, 0.4, 0.1, -1))
Y = c(1, 0.5, 1.2, 1, 0.5, 0, 1.1)

norm<-function(x){
  sum(x^2)
}

mu=rbind(c(0,0),c(1,1))

rX=array(0,c(NROW(X),2))

for (i in 1:7){
  for (m in 1:2){
    #rX[i,m]=exp(-(X[i,]-mu[m,])^2)
    rX[i,m]=exp(-norm(X[i,]-mu[m,]))
  }
}

rX

betaw=solve(t(rX)%*%rX)%*%t(rX)%*%Y
betaw
