# Let z∼U(−2,u) an uniformly distributed random variable between -2 and u.

# Let us consider the dataset of N=10 observations of z:
# DN=[1.3,−0.3,1.0,−1.3,0.4,−1.5,−0.9,−0.3,0.1,2.4]

# Suppose that u∈U={−1.5,−1.0,−0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0}

DN <- c(1.3,-0.3,1.0,-1.3,0.4,-1.5,-0.9,-0.3,0.1,2.4)
U <- c(-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0)

maxu <- 0
lik <- -1
for(u in U){
    likelihood <- 1
    for(data in DN){
        likelihood <- likelihood * (dunif(data, -2, u))
    }
    if(likelihood > lik){
        lik <- likelihood
        maxu <- u
    }
}
print(paste("u which maximize:", maxu))
