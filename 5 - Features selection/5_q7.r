load("FS.Rdata")
Xtr <- X
Ytr <- Y
lambda <- c(0, 1, 10, 100, 1000, 10000, 100000)
n <- 50
brmin <- Inf
lambdaopti <- 0
Eloo2=NULL
for(i in 1:n){
  Xi=Xtr[-i,]
  Yi=Ytr[-i]
  Ytsi=Ytr[i]
  Xtsi=Xtr[i,]

  Xi1=Xi[,1]
  for(j in 1:length(lambda)){
    br=solve(t(Xi1)%*%Xi1 + lambda[j]*diag(1)) %*%t(Xi1)%*%Yi
    if(br<brmin){
      lambdaopti = lambda[j]
    }
  }
  Eloo2=c(Eloo2,Ytsi-(br[1]+ br[2]*Xtsi[2]))
}
print(paste("Optimal lambda:", lambdaopti))
