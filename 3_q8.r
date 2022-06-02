# Let us consider the three models
# y=x
# y=−x
# y=β0+β1*x
#
# Compute the empirical risks (mean of squared residual errors MISEˆemp )for the three models

x	<- c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1)
y <- c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3, -0.7, 0.6, 0.3)

x_b <- c(-1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1)
y_b <- c(0.3, 0.6, -1, 0, 0.1, 0.3, -0.7, 0.6, 0.3)
x_loo	<- 1.3
y_loo <- -1.2
N <- 10

sse <- function(b0, b1, x, y) {
  return(sum((y-b0-(b1*x))^2))
}

mise_emp <- function(sse){
  return(sse/N)
}

eval_mod1 <- function(x, y, loo) {
  if(loo==FALSE){
    Sxy = sum((x-mean(x))*y)
    Sxx = sum((x-mean(x))*x)
    b1 = 0
    b0 = mean(y) - b1*mean(x)
  }
  else{
    b0 = 0.0555555555555556
    b1 = 0
  }
  mise_emp = mise_emp(sse(b0, b1, x, y))
  print(paste("MISEemp 1:", mise_emp))
  print(paste("Min b0 and b1:", b0, ",", b1))
}

eval_mod2 <- function(x, y, loo) {
  if(loo==FALSE){
    Sxy = sum((x-mean(x))*y)
    Sxx = sum((x-mean(x))*x)
    b1 = Sxy/Sxx
    b0 = mean(y) - b1*mean(x)
  }
  else{
    b0 = 0.0529274004683841
    b1 = -0.118266978922717
  }
  mise_emp = mise_emp(sse(b0, b1, x, y))
  print(paste("MISEemp 2:", mise_emp))
  print(paste("Min b0 and b1:", b0, ",", b1))
}


#eval_mod1(x, y, FALSE)
#eval_mod2(x, y, FALSE)

eval_mod1(x_b, y_b, FALSE)
eval_mod2(x_b, y_b, FALSE)

eval_mod1(x_loo, y_loo, TRUE)
eval_mod2(x_loo, y_loo, TRUE)
