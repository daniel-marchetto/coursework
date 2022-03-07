##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(MASS)
library(numDeriv)


##Data
rx <- read.csv(file = 'rx.dat', header = FALSE)
ry <- read.csv(file = 'ry.dat', header = FALSE)
colnames(rx) <- 'x'
colnames(ry) <- 'y'

#Test Regression
test <- cbind(ry,rx)
test.lm <- lm(y ~ x, data = test)
summary(test.lm)

w <- c(0)
for (i in (1:length(test[,1]))){
  w[i] <- exp((-test[i,2]^2) / 20)
}
testW.lm <- lm(y ~ x, data = test, weights = w)
summary(testW.lm)

#Cost Function
cost <- function(X,b,y){
  sum((X %*% b - y)^2 / (2 * length(y)))
}

costW <- function(W,X,b,y){
  t(X %*% b - y) %*% W %*% (X %*% b - y)
}

#Hyperparameters
learn <- 0.001
iterations <- 10000

#Initial COefficients and Matrices
b <- matrix(c(0,0), nrow = 2)
X <- as.matrix(cbind(1,rx))
y <- as.matrix(ry)
W <- diag( 1, length(X[,1]), length(X[,1]))
for (i in (1:length(X[,1]))){
  W[i,i] <- exp((-X[i,2]^2) / 20)
}

#Gradient Descent Regression
history <- data.frame(cost = double(),
                      b0 = double(),
                      b1 = double())

for (i in (1:iterations)){
  history[i,1] <- cost(X,b,y)
  history[i,2] <- t(b[1,])
  history[i,3] <- t(b[2,])
  gradient <- t(X) %*% (X %*% b - y) / length(y)
  b <- b - learn * gradient 
}

print(b)

plot(rx$x,ry$y,xlab = 'X', ylab = 'Y', main = 'Gradient Descent')
abline(a=b[1,1],b=b[2,1])

#Weighted Gradient Descent Regression
historyW <- data.frame(costW = double(),
                      b0 = double(),
                      b1 = double())
for (i in (1:iterations)){
  historyW[i,1] <- costW(W,X,b,y)
  historyW[i,2] <- t(b[1,])
  historyW[i,3] <- t(b[2,])
  gradient <- t(X) %*% W %*% (X %*% b - y) / length(y)
  b <- b - learn * gradient 
}

print(b)

plot(rx$x,ry$y,xlab = 'X', ylab = 'Y', main = 'Weighted Gradient Descent')
abline(a=b[1,1],b=b[2,1])
plot(1:iterations-1, historyW$cost, xlab = 'Iteration', ylab = 'Cost', main = 'Weighted Gradient Descent Cost')
