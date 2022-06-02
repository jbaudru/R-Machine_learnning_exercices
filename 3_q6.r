# Let us consider the three models
# y=x
# y=−x
# y=β0+β1*x
#
# Compute the empirical risks (mean of squared residual errors MISEˆemp )for the three models

x	<- c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9)
y <- c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3, -0.7)

sse <- function(b0, b1, x, y) {
  return(sum((y-b0-(b1*x))^2))
}

mise <- function(sse, N){
  return(sse/N)
}

eval_mod1 <- function(x, y) {
  mise = mise(sse(0, 1, x, y), length(x))
  print(paste("MISE 1:", mise))
  #return(x)
}

eval_mod2 <- function(x, y) {
  mise = mise(sse(0, -1, x, y), length(x))
  print(paste("MISE 2:", mise))
  #return(-x)
}

eval_mod3 <- function(x, y) {
  Sxy = sum((x-mean(x))*y)
  Sxx = sum((x-mean(x))*x)
  b1 = Sxy/Sxx
  b0 = mean(y) - b1*mean(x)

  mise = mise(sse(b0, b1, x, y), length(x))
  print(paste("MISE 3:", mise))
  #return(b0 + b1*x)
}

eval_mod1(x, y)
eval_mod2(x, y)
eval_mod3(x, y)
