m1 <- 0.5
s21 <- 0.25
m2 <- 1.5
s22 <- 1

DN <- c(1.3,-0.3,1.0, -1.3, 0.4, -1.5, -0.9, -0.3, 0.1, 2.4)
W1 <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

maxw <- 0
lik <- -1
for(w1 in W1){
    likelihood <- 1
    for(data in DN){
        likelihood <- likelihood * (w1*(dnorm(data,m1,s21)) + (1-w1)*(dnorm(data,m2,s22)))
    }
    if(likelihood > lik){
        lik <- likelihood
        maxw <- w1
    }
}
print(paste("w which maximize :", maxw))
