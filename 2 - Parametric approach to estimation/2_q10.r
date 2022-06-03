# Let z=w1*z1+w2*z2 where z1∼N(μ1,σ21) and z2∼N(μ2,σ22) and w2=1−w1.

#Let us consider the dataset of N=10 observations of z:
#DN=[1.3,−0.3,1.3,1.3,0.4,−1.5,−0.9,−0.3,0.0,2.4]

#Suppose that w1∈W1={0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0}

#Code a R script to find the value of w1∈W1 which maximises
#the likelihood of the dataset if μ1=−0.5,σ21=1,μ2=1,σ22=1.
m1 <- -0.5
s21 <- 1
m2 <- 1
s22 <- 1

DN <- c(1.3,-0.3,1.3,1.3,0.4,-1.5,-0.9,-0.3,0.0,2.4)
W1 <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

max <- 0
lik <- -1
for(w1 in W1){
    likelihood <- 1
    for(data in DN){
        likelihood <- likelihood * (w1*(dnorm(data,m1,s21)) + (1-w1)*(dnorm(data,m2,s22)))
    }
    if(likelihood > lik){
        lik <- likelihood
        max <- w1
    }
}
print(max)
