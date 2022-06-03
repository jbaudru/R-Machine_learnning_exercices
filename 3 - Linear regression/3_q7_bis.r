x1=c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1)
x2=c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3)
Xtr=cbind(numeric(length(x1))+1,x1, x2)
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3)

Xtr1=Xtr[,1]
Xtr2=Xtr[,1:2]
Xtr3=Xtr
# Cheveux dans la soupe concenant comment j'utilise les indices des x
# Plus clair dans la solution

betahatr1=solve(t(Xtr1)%*%Xtr1) %*%t(Xtr1)%*%Ytr
betahatr2=solve(t(Xtr2)%*%Xtr2) %*%t(Xtr2)%*%Ytr
betahatr3=solve(t(Xtr3)%*%Xtr3) %*%t(Xtr3)%*%Ytr

Yhatr1=Xtr2[,2]*Xtr3[,3]
Yhatr2=-Xtr2[,2]
Yhatr3=betahatr3[1]+betahatr3[2]*Xtr3[,2]+betahatr3[3]*Xtr3[,3]

MISEemp1=mean((Ytr-Yhatr1)^2)
MISEemp2=mean((Ytr-Yhatr2)^2)
MISEemp3=mean((Ytr-Yhatr3)^2)
print("TRAIN:")
print(paste("MISEemp 1:", MISEemp1))
print(paste("MISEemp 2:", MISEemp2))
print(paste("MISEemp 3:", MISEemp3))
