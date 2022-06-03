load("FS3.Rdata")

correlation_coef <- function(X, Y){
  co <- cov(X, Y, method = "spearman")
  deno <- sqrt(var(X)*var(Y))
  return (co/deno)
}

print(paste("Rho 1:", correlation_coef(X[,1], Y)))
print(paste("Rho 2:", correlation_coef(X[,2], Y)))
print(paste("Rho 3:", correlation_coef(X[,3], Y)))
print(paste("Rho 4:", correlation_coef(X[,4], Y)))
print(paste("Rho 5:", correlation_coef(X[,5], Y)))
