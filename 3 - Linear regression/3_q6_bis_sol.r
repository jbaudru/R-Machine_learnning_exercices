rm(list=ls())
Xtr=cbind(numeric(10)+1,c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1),c(0.6,0.1,0.5,-0.3,0.01,-0.05,0.15,-0.3,0.5,0.3))
Ytr=c(-1.2, 0.3, 0.6, -1, 0.5, 0.1, 0.3,-0.7,0.6,0.3)

Eloo=NULL
for (i in 1:10){
  Xi=Xtr[-i,]
  Yi=Ytr[-i]
  Ytsi=Ytr[i]
  Xtsi=Xtr[i,]
  betahatri=solve(t(Xi)%*%Xi) %*%t(Xi)%*%Yi
  Yhatsi = (betahatri[1]+ betahatri[2]*Xtsi[2]+ betahatri[3]*Xtsi[3])
  Eloo=c(Eloo,Ytsi-Yhatsi)
}

MISEloo=mean(Eloo^2)
cat("MISEloo=",MISEloo)
betahatr=solve(t(Xtr)%*%Xtr) %*%t(Xtr)%*%Ytr
e=Ytr-Xtr%*%betahatr
H=Xtr%*%solve(t(Xtr)%*%Xtr)%*%t(Xtr)
EPress<-e/(1-diag(H))
print(EPress-Eloo)
cat("MISEloo=",mean(Eloo^2), "MISEPress=",mean(EPress^2),"\n")
