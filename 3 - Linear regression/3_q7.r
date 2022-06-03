# Suppose we want to estimate the parameters of the linear model :
# y=β0+β1*x+w by using least-squares.

x	<- c(1.3, 1.0, 0.4, -0.9, 0.1)
y <- c(-0.3, -1.3, -1.5, -0.3, 2.4)
N <- length(x)

sse <- function(b0, b1, x, y) {
  return(sum((y-b0-(b1*x))^2))
}

mod_estimate <- function(x, y) {
    b0min <- Inf
    b1min <- Inf
    ssemin <- Inf
    for(i in 1:N){
      Sxy = sum((x-mean(x))*y)
      Sxx = sum((x-mean(x))*x)
      b1 = Sxy/Sxx
      b0 = mean(y) - b1*mean(x)
      sse <- sse(b0, b1, x, y)
      if(sse < ssemin){
        ssemin <- sse
        b0min <- b0
        b1min <- b1
      }
    }
    print(paste("Min b0 and b1:", b0, ",", b1))
}

mod_estimate(x, y)
