X = cbind(numeric(7)+1, c(0.1, 0, -0.3, 0.2, 0.4, 0.1, -1))
Y = c(1, 0.5, 1.2, 1, 0.5, 0, 1.1)

betahat=solve(t(X)%*%X) %*%t(X)%*%Y
betahat

YH = NULL
for(i in 1:7){
  Yhat = (betahat[1]+ betahat[2]*X[i,2])
  YH = cbind(YH, Yhat)
}

#Yhat = (betahat[1]+ betahat[2]*X[2])
png(file="Q11.png")
plot(X[,2], Y, xlab ="x", ylab ="y", col ="black", main="Linear regression")
lines(X[,2], YH, col="red", lwd=2)
