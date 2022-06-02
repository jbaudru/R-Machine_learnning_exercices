# z with Normal distribution N(μ,σ2), where μ=1 and σ=1.
# N=100
# By implementing a Monte Carlo simulation with 10000 trials in R,
# the student should compute the bias, variance and MSE of the following four estimators of the ratio μ/σ

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
m1 <- 1
s21 <- 1

EST1 = NULL
EST2 = NULL
EST3 = NULL
EST4 = NULL
Z = NULL
for (i in 1:R){
  z=rnorm(N,m1,s21) # Take a sample of 100 elem in the distrib
  Z <- c(Z,z)

  mhat = mean(z)
  s2hat = sd(z)

  estim1 = mhat/sqrt(s2hat)
  EST1 <- c(EST1, estim1)

  estim2 = sum(z)
  EST2 <- c(EST2, estim2)

  estim3 = sum((z-mhat)^3)/(N*sqrt(s2hat)^3)
  EST3 <- c(EST3, estim3)

  estim4 = sum((z-mhat)^4)/(N*sqrt(s2hat)^4)
  EST4 <- c(EST4, estim4)
}

TRUEEST <- mean(Z)/sqrt(sd(Z))

print(paste("BIAIS EST1:", abs(biais(EST1, TRUEEST))))
print(paste("VAR EST1:", var(EST1, TRUEEST)))
print(paste("MSE EST1:", mse(EST1, TRUEEST)))
print("")
print(paste("BIAIS EST2:", abs(biais(EST2, TRUEEST))))
print(paste("VAR EST2:", var(EST2, TRUEEST)))
print(paste("MSE EST2:", mse(EST2, TRUEEST)))
print("")
print(paste("BIAIS EST3:", abs(biais(EST3, TRUEEST))))
print(paste("VAR EST3:", var(EST3, TRUEEST)))
print(paste("MSE EST3:", mse(EST3, TRUEEST)))
print("")
print(paste("BIAIS EST4:", abs(biais(EST4, TRUEEST))))
print(paste("VAR EST4:", var(EST4, TRUEEST)))
print(paste("MSE EST4:", mse(EST4, TRUEEST)))
