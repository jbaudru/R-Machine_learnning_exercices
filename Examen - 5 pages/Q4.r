X = cbind(c(-0.2, 0.1, 1, 0.1, -0.4, 0.1, 1), c(0.1, 0, -0.3, 0.2, 0.4, 0.1, -1))
Y = c(1, 0.5, 1.2, 1, 0.5, 0, 1.1)

betahat=solve(t(X)%*%X) %*%t(X)%*%Y
betahat

Yhat = (0+ betahat[1]*X[,1] + betahat[2]*X[,2])

MISEemp1=mean((Y-Yhat)^2)
print(paste("MISEemp : ", MISEemp1))

for (i in 1:2){
  cor = cor(X[,i], Y)
  print(paste("Cor (x", i,", y) :",cor))
}
