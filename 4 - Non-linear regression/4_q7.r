# y=sin(x)+w  where
# w is a Normal random variable with zero mean and variance equal to 0.5
# x  is a Uniform random variable with range [-1,1]
# the training set is made of N=20 observations

# M1: y^=β^0+β^1x
# M2: y^=β^0+β^1x+β^2x2

# Code an R script to compute by Monte Carlo simulation (with 10000 trials)
# the variance and the  squared bias of the prediction y^ pour x=0
# for the two models.

biais_model <- function(yhat, y){
  #return(mean(mean(yhat)-y))
  return ((mean(yhat)-sin(0))^2)
}

var_model <- function(yhat){ # Error ?
  return (var(yhat))
  #return(mean((yhat-mean(yhat))^2))
}

N <- 20
R <- 10000

f <- function(x, w){
  return(sin(x)+w)
}

Y= NULL
YHAT1 = NULL
YHAT2 = NULL
for (i in 1:R){
  x <- runif(N, -1, 1)
  Xtr=cbind(numeric(10)+1,x,x^2)
  w <- rnorm(N, 0, 0.5)
  y <- f(x, w)
  Y <- c(Y,y)

  # Model 1
  Xtr1=Xtr[,1:2] # Col 1 à 2 = (num, x)
  betahatr1=solve(t(Xtr1)%*%Xtr1) %*%t(Xtr1)%*%y
  Yhatr1=betahatr1[1]+betahatr1[2]*Xtr1[,2]
  Yhatr1=betahatr1[1]+betahatr1[2]*0 # Enoncé : X=0 for the models
  YHAT1 <- c(YHAT1,Yhatr1)

  # Model 2
  Xtr2=Xtr # Col 1 à 3 = (num, x, x^2)
  betahatr2=solve(t(Xtr2)%*%Xtr2) %*%t(Xtr2)%*%y
  Yhatr2=betahatr2[1]+betahatr2[2]*Xtr2[,2]+betahatr2[3]*Xtr2[,3]
  Yhatr2=betahatr2[1]+betahatr2[2]*0+betahatr2[3]*0 # Enoncé : X=0 for the models
  YHAT2 <- c(YHAT2,Yhatr2)
}

print(paste("VAR Model 1:", var_model(YHAT1)))
print(paste("BIAIS^2 Model 1:", biais_model(YHAT1, Y)^2))

print(paste("VAR Model 2:", var_model(YHAT2)))
print(paste("BIAIS^2 Model 2:", biais_model(YHAT2, Y)^2))
