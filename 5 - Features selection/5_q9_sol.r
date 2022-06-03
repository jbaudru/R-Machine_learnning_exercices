load("FS.Rdata")
n=NCOL(X)
corXY=NULL
for (j in 1:n){
  corXY=c(corXY,abs(cor(X[,j],Y)))
}
print(sort(corXY,decre=TRUE,index=TRUE)$ix[1:5])
