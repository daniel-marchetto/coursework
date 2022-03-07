##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)

##Libraries
library(MASS) 
library(rsample) #Not a base package. Need to install.
library(ggplot2)


##Metropolis Hastings Exponential
MHexp <- function(TRIALS) {
  mesh <- seq(0, 1, 0.01)
  sigma <- 0.05
  t = TRIALS
  S = 5
  n = 20
  x <- matrix(NA, nrow = t+1)
  x[1,] <- sample(mesh,1)
  
  for (i in 2:(t+1)){
    prob <- exp(-(mesh - x[i-1,])^2 / (2 * sigma^2))
    probNorm <- prob / sum(prob)
    proposed <- sample(mesh, size = 1, replace = T, prob = probNorm)
    numerator <- (proposed^S * (1 - proposed)^(n-S) * cos(4 * pi * proposed)^2 )
    denominator <- (x[i-1,]^S * (1 - x[i-1,])^(n-S) * cos(4 * pi * x[i-1,])^2 )
    alpha = min(1, numerator / denominator)
    
    u = runif(1)
    
    if (u < alpha){
      x[i,] <- proposed
    }
    else{
      x[i,] <- x[i-1,]
    }
  }
  return(x)
}


#Plots
x100 <- as.data.frame(MHexp(100))
x500 <- as.data.frame(MHexp(500))
x1000 <- as.data.frame(MHexp(1000))
x5000 <- as.data.frame(MHexp(5000))

S = 5
n = 20
mesh <- seq(0, 1, 0.01)
prob <- 2 * mesh^S * (1-mesh)^(n-S) * cos(4 * pi * mesh)^2
probNorm <- prob / sum(prob)
true <- as.data.frame(sample(mesh, size = 10000, replace = T, prob = probNorm))

plot1 <- ggplot() +
  geom_density(data = x100, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (100 Trials)") +
  labs(x = "values")
plot2 <- ggplot() +
  geom_density(data = x500, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (500 Trials)") +
  labs(x = "values")
plot3 <- ggplot() +
  geom_density(data = x1000, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (1000 Trials)") +
  labs(x = "values")
plot4 <- ggplot() +
  geom_density(data = x5000, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (5000 Trials)") +
  labs(x = "values")

plot1
plot2
plot3
plot4


#Estimates
sum(x) / length(x)
sum((x - 0.5)^2) / length(x)


##Metropolis Hastings Uniform
MHuni <- function(TRIALS) {
  mesh <- seq(0, 1, 0.01)
  sigma <- 0.05
  t = TRIALS
  S = 5
  n = 20
  x <- matrix(NA, nrow = t+1)
  x[1,] <- sample(mesh,1)
  
  for (i in 2:(t+1)){
    prob <- exp(-(mesh - x[i-1,])^2 / (2 * sigma^2))
    probNorm <- prob / sum(prob)
    proposed <- sample(mesh, size = 1, replace = T, prob = probNorm)
    numerator <- (proposed^S * (1 - proposed)^(n-S))
    denominator <- (x[i-1,]^S * (1 - x[i-1,])^(n-S))
    alpha = min(1, numerator / denominator)
    
    u = runif(1)
    
    if (u < alpha){
      x[i,] <- proposed
    }
    else{
      x[i,] <- x[i-1,]
    }
    
  }
  return(x)
}


#Plots
x100 <- as.data.frame(MHuni(100))
x500 <- as.data.frame(MHuni(500))
x1000 <- as.data.frame(MHuni(1000))
x5000 <- as.data.frame(MHuni(5000))

S = 5
n = 20
mesh <- seq(0, 1, 0.01)
prob <- 2 * mesh^S * (1-mesh)^(n-S)
probNorm <- prob / sum(prob)
true <- as.data.frame(sample(mesh, size = 10000, replace = T, prob = probNorm))

plot5 <- ggplot() +
  geom_density(data = x100, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (100 Trials)") +
  labs(x = "values")
plot6 <- ggplot() +
  geom_density(data = x500, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (500 Trials)") +
  labs(x = "values")
plot7 <- ggplot() +
  geom_density(data = x1000, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (1000 Trials)") +
  labs(x = "values")
plot8 <- ggplot() +
  geom_density(data = x5000, aes(x=V1, color = "MH")) +
  geom_density(data = true, aes(x=true$`sample(mesh, size = 10000, replace = T, prob = probNorm)`, color = "True")) +
  scale_colour_manual(name="Legend", values = c("MH" = "blue", "True" = "red")) +
  labs(title = "Generated v True (5000 Trials)") +
  labs(x = "values")

plot5
plot6
plot7
plot8


#Estimates
x <- MHuni(10000)
sum(x) / length(x)
sum((x - 0.5)^2) / length(x)


