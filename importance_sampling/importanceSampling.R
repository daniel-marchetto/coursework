##Importance Sampling
mu = 1
z = 3
N = 100


##Direct
direct <- function(mu,z,N){
  temp <- rnorm(n = N, mean = mu, sd = 1)
  return(sum(temp > z) / N)
}


##Indirect
indirect <- function(mu,z,N){
  temp <- rnorm(n = N, mean = z, sd = 1)
  tail <- 0
  for (i in 1:N){
    prob <- sum(temp[i]>z) * exp(0.5*((temp[i]-z)^2 - (temp[i]-mu)^2))
    tail <- tail + prob
  }
  return(tail / N)
}


##MC Estimation
B = 500
sampMC <- data.frame(direct = numeric(B),
                     indirect = numeric(B))
for (i in 1:B){
  sampMC[i,1] <- direct(mu,z,N)
  sampMC[i,2] <- indirect(mu,z,N)
}
colMeans(sampMC)
cov(sampMC)
