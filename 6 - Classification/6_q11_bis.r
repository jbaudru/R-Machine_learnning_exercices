load("EXAM.Rdata")
D=Q6.G1.D

Ip=which(D[,3]==1)  # Index Y == 1
In=which(D[,3]==-1) # Index Y == -1

classes=c("-1","1")
N=NROW(D)
n=2
X=D[,1:n] # Inputs
Y=D[,n+1] # Output

beta0=0
beta=c(-1,1)

# Gradient-based descent
Niter=5 # Nombre itération
eta=0.01 # Learning rate
for (k in 1:Niter){
  M=NULL
  for (i in 1:N){
    xi=X[i,]
    yi=Y[i]
    gammai=yi*(t(xi)%*%beta+beta0)
    if (gammai<0){ # If gamma < 0, then we have misclassified the point xi
      M=c(M,i)
    }
  }

  cat("# misclassified points after",k-1, "iteration steps =",length(M),"\n")

  gradbeta=-apply(Y[M]*X[M,],2,sum)
  gradbeta0=-sum(Y[M])
  beta=beta-eta*gradbeta
  beta0=beta0-eta*gradbeta0
  sx1=seq(-2,2,by=0.01)
  lines(sx1,-beta[1]/beta[2]*sx1-beta0/beta[2])
}
