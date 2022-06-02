# Monte Carlo simulation pour calculer f(E[x]) et E[f(x)]
# avec x = Uniform(-2,5) et f(x) = x^2-x
f <- function(x){
  return (x^2-x)
}

R <- 10000
a <- -2
b <- 5

X = NULL
FX = NULL

for (i in 1:R){
  x=runif(1,a,b) # Uniforme distribution
  y <- f(x)
  # Add to list
  X <- c(X, x)
  FX <- c(FX, y)
}

mX = mean(X)
print(paste("f(E[x])", f(mX)))
print(paste("E[f(x)]", mean(FX)))
# On voit bien que la propriété f(E[x]) < E[f(x)] est respectée

# EXECUTION : Rscript.exe file.r
