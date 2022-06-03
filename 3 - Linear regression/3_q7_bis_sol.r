rm(list=ls())
X=cbind(numeric(10)+1,
        c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1),
        c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3))
Y=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3)

betahat=solve(t(X)%*%X) %*%t(X)%*%Y
Yhat1=X[,2]+X[,3]
Yhat2=-X[,2]
Yhat3=betahat[1]+X[,2]*betahat[2]+X[,3]*betahat[3]

MISEemp1=mean((Y-Yhat1)^2)
MISEemp2=mean((Y-Yhat2)^2)
MISEemp3=mean((Y-Yhat3)^2)
print(paste("MISEemp 1:", MISEemp1))
print(paste("MISEemp 2:", MISEemp2))
print(paste("MISEemp 3:", MISEemp3))
