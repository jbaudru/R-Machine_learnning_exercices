rm(list=ls())
x1=c(1.3,-1.2,0.5,1.0,0.3,0,0.4,-0.9,-0.2,-0.1)
x2=c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3)
Xtr=cbind(numeric(length(x1))+1,x1, x2)
Ytr=c(-1.2, 0.3, 0.6, -1, 0.5, 0.1, 0.3,-0.7,0.6,0.3)
# Y avait une erreur dans mais output mdr c'est pour ça que j'avais pas la bonne réponse

Eloo1=NULL
for (i in 1:length(x1)){
  Xi=Xtr[-i,]
  Yi=Ytr[-i]
  Ytsi=Ytr[i]
  Xtsi=Xtr[i,]
  betahatr3=solve(t(Xi)%*%Xi) %*%t(Xi)%*%Yi
  Yhatsi = betahatr3[1]+ betahatr3[2]*Xtsi[2]+ betahatr3[3]*Xtsi[3]
  Eloo1=c(Eloo1,Ytsi-Yhatsi)

}
MISEloo1=mean(Eloo1^2)
print(paste("MISEloo 1:", MISEloo1))
