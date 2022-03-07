##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(MASS)
library(numDeriv)


##Data
logitx <- read.table(file = 'logit-x.dat', quote="\"", comment.char="", header = FALSE)
logity <- read.table(file = 'logit-y.dat', quote="\"", comment.char="", header = FALSE)
colnames(logitx) <- c('x1','x2')
colnames(logity) <- 'y'

#Test LR
test <- cbind(logitx,logity)
test.lr <- glm(y ~ 0 + x1 + x2, data = test, family = 'binomial')
summary(test.lr)

#Functions
logistic <- function(x){
  1 / (1 + exp(-x))
}

cost <- function(X,coef,y){
  sum(y * log(logistic(X %*% coef)) + (1 - y) * log(1 - logistic(X %*% coef)))
}

#Hyperparameters
learn <- 1

#Initial Coefficients and Matrices
coef <- matrix(c(0,0), nrow = 2)
X <- as.matrix(logitx)
y <- as.matrix(logity)


#Logistic Regression
history <- data.frame(cost= double(),
                      a = double(),
                      b = double())
change <- matrix(c(1,1))
iterations <- 0
while (iterations < 10){
  history[iterations+1,1] <- -cost(X,coef,y)
  history[iterations+1,2] <- t(coef[1,])
  history[iterations+1,3] <- t(coef[2,])
  gradient <- t(X) %*% (logistic(X %*% coef) - y)
  hes <- hessian(cost, coef, X = X, y = y)
  oldCoef <- coef
  coef <- coef + learn * ginv(hes) %*% gradient
  change <- (coef - oldCoef)^2
  iterations <- iterations + 1
}

print(coef)

plot(1:iterations-1, history$cost, xlab = 'Iteration', ylab = 'log Likelihood', main = 'Logit Regression Cost')
plot(1:iterations-1, history$a)
plot(1:iterations-1, history$b)
