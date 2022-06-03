x=c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1)
Xtr=cbind(numeric(10)+1,x,x^2)
Ytr=c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3,-0.7,0.6,0.3)

# %*% = Multiplication matricielle

Xtr1=Xtr[,1]
betahatr1=solve(t(Xtr1)%*%Xtr1) %*%t(Xtr1)%*%Ytr
Yhatr1=betahatr1[1]

Xtr2=Xtr[,1:2]
betahatr2=solve(t(Xtr2)%*%Xtr2) %*%t(Xtr2)%*%Ytr
Yhatr2=betahatr2[1]+betahatr2[2]*Xtr2[,2]

Xtr3=Xtr
betahatr3=solve(t(Xtr3)%*%Xtr3) %*%t(Xtr3)%*%Ytr
Yhatr3=betahatr3[1]+betahatr3[2]*Xtr3[,2]+betahatr3[3]*Xtr3[,3]

MISEemp1=mean((Ytr-Yhatr1)^2)
MISEemp2=mean((Ytr-Yhatr2)^2)
MISEemp3=mean((Ytr-Yhatr3)^2)
print(paste("MISEemp 1:", MISEemp1))
print(paste("MISEemp 2:", MISEemp2))
print(paste("MISEemp 3:", MISEemp3))

Eloo1=NULL
Eloo2=NULL
Eloo3=NULL
for (i in 1:10){
  Xi=Xtr[-i,]
  Yi=Ytr[-i]
  Ytsi=Ytr[i]
  Xtsi=Xtr[i,]

  Xi1=Xi[,1]
  betahatr1=solve(t(Xi1)%*%Xi1) %*%t(Xi1)%*%Yi
  Eloo1=c(Eloo1,Ytsi-betahatr1 )

  Xi2=Xi[,1:2]
  betahatr2=solve(t(Xi2)%*%Xi2) %*%t(Xi2)%*%Yi
  Eloo2=c(Eloo2,Ytsi-(betahatr2[1]+ betahatr2[2]*Xtsi[2]))

  Xi3=Xi[,1:3]
  betahatr3=solve(t(Xi3)%*%Xi3) %*%t(Xi3)%*%Yi
  Eloo3=c(Eloo3,Ytsi-(betahatr3[1]+ betahatr3[2]*Xtsi[2]+ betahatr3[3]*Xtsi[3]))

}
MISEloo1=mean(Eloo1^2)
MISEloo2=mean(Eloo2^2)
MISEloo3=mean(Eloo3^2)
print(paste("MISEloo 1:", MISEloo1))
print(paste("MISEloo 2:", MISEloo2))
print(paste("MISEloo 3:", MISEloo3))
