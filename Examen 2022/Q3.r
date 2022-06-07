rm(list=ls())
load("EXAM.1s.22.Rdata")
set.seed(0)

biais <- function(thetahat, theta){
  return(mean(thetahat)-theta)
}

var <- function(thetahat, theta){
  return(mean((thetahat-mean(thetahat))^2))
}

mse <- function(thetahat, theta){
  return(biais(thetahat, theta)^2+var(thetahat, theta))
}

R <- 10000
N <- 100
m1 <- 0
s21 <- 2

EST1 = NULL
EST2 = NULL
EST3 = NULL
EST4 = NULL
Z = NULL
for (i in 1:R){
  z=rnorm(N,m1,s21) # Take a sample of 100 elem in the distrib
  Z <- c(Z,z)

  estim1 = sd(z)
  EST1 <- c(EST1, estim1)

  estim2 = max(z) - min(z)
  EST2 <- c(EST2, estim2)

  estim3 = sum(abs(z-mean(z)))/N
  EST3 <- c(EST3, estim3)

  estim4 = median(z)
  EST4 <- c(EST4, estim4)
}

TRUEEST <- sd(Z)

print(paste("ABS BIAIS EST1:", abs(biais(EST1, TRUEEST))))
print(paste("VAR EST1:", var(EST1, TRUEEST)))
print(paste("MSE EST1:", mse(EST1, TRUEEST)))
print("")
print(paste("ABS BIAIS EST2:", abs(biais(EST2, TRUEEST))))
print(paste("VAR EST2:", var(EST2, TRUEEST)))
print(paste("MSE EST2:", mse(EST2, TRUEEST)))
print("")
print(paste("ABS BIAIS EST3:", abs(biais(EST3, TRUEEST))))
print(paste("VAR EST3:", var(EST3, TRUEEST)))
print(paste("MSE EST3:", mse(EST3, TRUEEST)))
print("")
print(paste("ABS BIAIS EST4:", abs(biais(EST4, TRUEEST))))
print(paste("VAR EST4:", var(EST4, TRUEEST)))
print(paste("MSE EST4:", mse(EST4, TRUEEST)))
