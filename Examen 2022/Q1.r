rm(list=ls())
load("EXAM.1s.22.Rdata")
set.seed(0)

data <- Q1.G2.D

# P(x4=0 | x1=1, X2=0, x3=0)
# = P(x4=0, x1=1, x2=0, x3=0) / P(x1=1, x2=0, x3=0)
sum_prob1 <- 0
sum_prob2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x4']==0 && data[i, 'x1']==1 && data[i, 'x2']==0 && data[i, 'x3']==0){
    sum_prob1 <- sum_prob1 + data[i, 'prob']
  }
}
for (i in 1:nrow(data)){
  if(data[i, 'x1']==1 && data[i, 'x2']==0 && data[i, 'x3']==0){
    sum_prob2 <- sum_prob2 + data[i, 'prob']
  }
}
print(paste("p(x4=0 | x1=1, X2=0, x3=0):",sum_prob1/sum_prob2))


# P(x4=0 | x1=0, X2=0, x3=0)
# = P(x4=0, x1=0, x2=0, x3=0) / P(x1=0, x2=0, x3=0)
sum_prob1 <- 0
sum_prob2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x4']==0 && data[i, 'x1']==0 && data[i, 'x2']==0 && data[i, 'x3']==0){
    sum_prob1 <- sum_prob1 + data[i, 'prob']
  }
}
for (i in 1:nrow(data)){
  if(data[i, 'x1']==0 && data[i, 'x2']==0 && data[i, 'x3']==0){
    sum_prob2 <- sum_prob2 + data[i, 'prob']
  }
}
print(paste("P(x4=0 | x1=0, X2=0, x3=0):",sum_prob1/sum_prob2))

# P(x1=0, x2=0)
px1x2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x1']==0 && data[i, 'x2']==0){
    px1x2 <- px1x2 + data[i, 'prob']
  }
}
print(paste("P(x1=0, x2=0):",px1x2))

# P(x4=0, x1=0)
px1x4 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x4']==0 && data[i, 'x1']==0){
    px1x4 <- px1x4 + data[i, 'prob']
  }
}
print(paste("P(x4=0, x1=0):",px1x4))

# P(x1=1)
px11 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x1']==1){
    px11 <- px11 + data[i, 'prob']
  }
}
print(paste("p(x1=1) :",px11))


# P(x1=0)
px10 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x1']==0){
    px10 <- px10 + data[i, 'prob']
  }
}
print(paste("p(x1=0) :",px10))

# P(x2=1)
px21 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x2']==1){
    px21 <- px21 + data[i, 'prob']
  }
}
print(paste("p(x2=1) :",px21))

# P(x2=0)
px21 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x2']==0){
    px21 <- px21 + data[i, 'prob']
  }
}
print(paste("p(x2=0) :",px21))

# P(x4=0)
px40 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x4']==0){
    px40  <- px40 + data[i, 'prob']
  }
}
print(paste("p(x4=0) :",px40))

# P(x4=1)
px40 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x4']==1){
    px40  <- px40 + data[i, 'prob']
  }
}
print(paste("p(x4=1) :",px40))
