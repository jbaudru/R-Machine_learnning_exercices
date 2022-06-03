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


x1ts=c(1.6, -1.1, 1.1, 0.9, -0.1)
x2ts=c(0.5, -0.1, -0.2, 0.3, -0.3)
Xts=cbind(numeric(length(x1ts))+1,x1ts, x2ts)
Yts=c(-1.4, -0.3, -1.0, 0.7, -0.3)

Xts1=Xts[,1]
Xts2=Xts[,1:2]
Xts3=Xts
# Cheveux dans la soupe concenant comment j'utilise les indices des x
# Plus clair dans la solution

betahats1=solve(t(Xts1)%*%Xts1) %*%t(Xts1)%*%Yts
betahats2=solve(t(Xts2)%*%Xts2) %*%t(Xts2)%*%Yts
betahats3=solve(t(Xts3)%*%Xts3) %*%t(Xts3)%*%Yts

Yhats1=Xts2[,2]*Xts3[,3]
Yhats2=-Xts2[,2]
Yhats3=betahats3[1]+betahats3[2]*Xts3[,2]+betahats3[3]*Xts3[,3]

MISEemp1=mean((Ytr-Yhats1)^2)
MISEemp2=mean((Ytr-Yhats2)^2)
MISEemp3=mean((Ytr-Yhats3)^2)
print("TEST:")
print(paste("MISEemp 1:", MISEemp1))
print(paste("MISEemp 2:", MISEemp2))
print(paste("MISEemp 3:", MISEemp3))
