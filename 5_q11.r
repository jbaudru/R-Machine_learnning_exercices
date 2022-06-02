load("FS5.Rdata")
Xtr <- X
Ytr <- Y

MISE_loo <- function(X, Y){
  N=length(Y)
  Eloo1<-NULL
  for (i in 1:N){
    betahat=solve(t(X)%*%X) %*%t(X)%*%Y
    err=Y-X%*%betahat
    Eloo1<-c(Eloo1, err)
  }
  return(mean(Eloo1^2))
}

correlation_coef <- function(X, Y){
  co <- cov(X, Y, method = "spearman")
  deno <- sqrt(var(X)*var(Y))
  return (co/deno)
}

ranking_corr_selection <-function(X, Y, N){ # N = number of features to select
  RHO = NULL
  index_to_keep = NULL
  for (i in 1:ncol(X)){
    rho <- correlation_coef(X[,i], Y)
    RHO=cbind(RHO, rho)
  }
  for (i in 1:length(RHO)){
    if(is.null(index_to_keep)){
      val = max(RHO)
    }
    else{
      val = max(RHO[-c(index_to_keep)])
    }
    ind = match(c(val), RHO)
    index_to_keep = cbind(index_to_keep, ind)
  }
  return(index_to_keep[1:N])
}
# Least square
print("All features")
print(paste("Training MISEloo:", MISE_loo(Xtr, Ytr)))
print(paste("Testing MISEloo:", MISE_loo(Xts, Ytr)))

ind_to_keep = ranking_corr_selection(X, Y, 5)
print("Best features:")
print(ind_to_keep)
Xtrnew <- Xtr[ind_to_keep]
Ytrnew <- Ytr[ind_to_keep]
Xtsnew <- Xts[ind_to_keep]
Ytsnew <- Yts[ind_to_keep]

print("5 best features")
print(paste("Training MISEloo:", MISE_loo(Xtrnew, Ytrnew)))
print(paste("Testing MISEloo:", MISE_loo(Xtsnew, Ytsnew)))
