load(".RData")
load("EXAM.Rdata")

data <- Q1.G1.D
data

# P(y=no | x1=yes, x2=no, x3=no )
# P(y=no, x1=yes, x2=no, x3=no ) / P(x1=yes, x2=no, x3=no )
sum_prob <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x1']=='yes' && data[i, 'x2']=='no' && data[i, 'x3']=='no'){
    sum_prob <- sum_prob + data[i, 'prob']
  }
}
print(data[15,5]/sum_prob)

# P(x3=no | x1=yes, y=no )
# = P(x3=no, x1=yes, y=no ) / P(x1=yes, y=no )
sum_prob1 <- 0
sum_prob2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x3']=='no' && data[i, 'x1']=='yes' && data[i, 'y']=='no'){
    sum_prob1 <- sum_prob1 + data[i, 'prob']
  }
}
for (i in 1:nrow(data)){
  if(data[i, 'x1']=='yes' && data[i, 'y']=='no'){
    sum_prob2 <- sum_prob2 + data[i, 'prob']
  }
}
print(sum_prob1/sum_prob2)

# P(x2=no | x1=yes, y=no)
# = P(x2=no, x1=yes, y=no) / # P(x1=yes, y=no)
sum_prob1 <- 0
sum_prob2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x2']=='no' && data[i, 'x1']=='yes' && data[i, 'y']=='no'){
    sum_prob1 <- sum_prob1 + data[i, 'prob']
  }
}
for (i in 1:nrow(data)){
  if(data[i, 'x1']=='yes' && data[i, 'y']=='no'){
    sum_prob2 <- sum_prob2 + data[i, 'prob']
  }
}
print(sum_prob1/sum_prob2)

# P(x1=no | x2=no, x3=no)
# = P(x1=no, x2=no, x3=no) / P(x2=no, x3=no)
sum_prob1 <- 0
sum_prob2 <- 0
for (i in 1:nrow(data)){
  if(data[i, 'x1']=='no' && data[i, 'x2']=='no' && data[i, 'x3']=='no'){
    sum_prob1 <- sum_prob1 + data[i, 'prob']
  }
}
for (i in 1:nrow(data)){
  if(data[i, 'x2']=='no' && data[i, 'x3']=='no'){
    sum_prob2 <- sum_prob2 + data[i, 'prob']
  }
}
print(sum_prob1/sum_prob2)
