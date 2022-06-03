rm(list=ls())
Xtr=cbind(numeric(10)+1,
        c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1),
        c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3))
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3)

betahatr=solve(t(Xtr)%*%Xtr) %*%t(Xtr)%*%Ytr

Yhatr1=Xtr[,2]+Xtr[,3]
Yhatr2=-Xtr[,2]
Yhatr3=betahatr[1]+Xtr[,2]*betahatr[2]+Xtr[,3]*betahatr[3]

MISEemp1=mean((Ytr-Yhatr1)^2)
MISEemp2=mean((Ytr-Yhatr2)^2)
MISEemp3=mean((Ytr-Yhatr3)^2)
print("TRAIN:")
print(paste("MISEemp 1:", MISEemp1))
print(paste("MISEemp 2:", MISEemp2))
print(paste("MISEemp 3:", MISEemp3))

Xts=cbind(numeric(5)+1,
          c(1.6,-1.1,1.1,0.9,-0.1),
          c(0.5,-0.1,-0.2,0.3,-0.3))
Yts=c(-1.4, -0.3,  -1, 0.7,-0.3)

Yhats1=Xts[,2]+Xts[,3]
Yhats2=-Xts[,2]
Yhats3=betahatr[1]+Xts[,2]*betahatr[2]+Xts[,3]*betahatr[3]

MISEts1=mean((Yts-Yhats1)^2)
MISEts2=mean((Yts-Yhats2)^2)
MISEts3=mean((Yts-Yhats3)^2)
print("TEST:")
print(paste("MISEemp 1:", MISEts1))
print(paste("MISEemp 2:", MISEts2))
print(paste("MISEemp 3:", MISEts3))
