##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(R.matlab) #Needed to import data. Not a base package. Make sure to install.
library(ggplot2)
library(reshape2)


##DATA IMPORT
dataRaw <- readMat('data.mat')
temp <- lapply(dataRaw, unlist, use.names=FALSE)
temp2 <- as.matrix(cbind(temp$data))
dataProcessed <- t(temp2)

labelRaw <- readMat('label.mat')
temp <- lapply(labelRaw, unlist, use.names=FALSE)
temp2 <- as.matrix(cbind(temp$trueLabel))
labelTrue <- t(temp2)


##DATA VISUALIZATION
two <- matrix(as.matrix(dataProcessed[1,]), nrow = 28, ncol = 28, byrow = T)
png('two.png')
image(two[1:28,28:1])
dev.off()

six <- matrix(as.matrix(dataProcessed[1990,]), nrow = 28, ncol = 28, byrow = T)
png('six.png')
image(six[1:28,28:1])
dev.off()


##GMM FUNCTION
myGMM <- function(data,k,r){
  #Initialize Parameters
  PI <- matrix(1/k,nrow = k, ncol = 1)
  
  MU <- t(matrix(rnorm(ncol(data)*k, mean=0, sd=.1), ncol = k))
  
  SIGMA <- list()
  for (i in 1:k) {
    SIGMA[[i]] <- diag(ncol(data))
  }
  
  TAU <- matrix(nrow = nrow(data), ncol = k)
  
  logl <- data.frame(Iteration = numeric(),
                     logLikelihood = numeric())
  iteration <- 0
  change <- 1
  
  
  while (iteration <= 50) {
    ##Expectation
    
    #Low Rank Approximation 
    MEAN <- list()
    EIGENVALUES <- list()
    DATA <- list()
    for (i in 1:k) {
      temp <- eigen(SIGMA[[i]])
      EIGENVALUES[[i]] <- temp$values[1:r]
      MEAN[[i]] <- t(temp$vectors[,1:r]) %*% MU[i,]
      DATA[[i]] <- t(t(temp$vectors[,1:r]) %*% t(data))
    }
    that <- matrix(nrow=nrow(data),ncol=k)
    for(i in 1:k){
      difference <- sweep(DATA[[i]],2,MEAN[[i]])
      temp <- difference^2
      temp2 <- t(t(temp) / EIGENVALUES[[i]])
      m <- rowSums(temp2)
      temp <- 1 / sqrt(EIGENVALUES[[i]])
      D <- prod(temp)
      for (j in 1:nrow(data)) {
        that[j,i] <- PI[i,] * D * exp((-0.5)*m[j]) 
      }
    }
    C <- that[,1] + that[,2]
    TAU <- that / C
    
    #Logging the likelihood for each iteration
    logl[iteration+1,1] <- iteration + 1 
    logl[iteration+1,2] <- sum(log((that[,1] + that[,2])))
    
    
    #Maximization
    #Updates PI
    PI <- matrix(colMeans(TAU)) 
    
    oldMU <- MU
    #Updates MU
    for (i in 1:k) {
      numerator <- colSums(TAU[,i] * data)
      denominator <- sum(TAU[,i])
      output <- numerator / denominator
      MU[i,] <- t(as.matrix(output))
    }
    
    #Updates SIGMA
    for (i in 1:k) {
      difference <- sweep(data, 2, MU[i,])
      transform <- TAU[,i] * difference
      numerator <- t(transform) %*% transform
      denominator <- sum(TAU[,i])
      output <- numerator / denominator
      SIGMA[[i]] <- output
    }
    
    
    iteration <- iteration + 1
    print(iteration)
    
    #Track change and terminate if no change.
    change <- sum(MU - oldMU)^2
    if (change < 0.0001) {
      print('Converged.')
      break
    }
  }
  Foutput <- list(TAU,PI,MU,logl)
  return(Foutput)
}


##GMM PCA Case
k = 2 #Number of Gaussians
r = 5 #Select dimensionality for E step. For low rank approx.
data <- dataProcessed

#PCA (Notice that PCA dimension is same as low rank approx for full reconstruction.)
temp <- eigen(cov(data))
temp2 <- temp$vectors
temp3 <- t(t(temp2[,1:r]) %*% t(data))
data <- temp3

#GMM Use
savePCA <- myGMM(data,k,r)

#Fitted Gaussians
componentWeights <- as.data.frame(savePCA[[2]]) #weights
meanVectors <- as.data.frame(savePCA[[3]] %*% t(temp2[,1:r]) )
meanTwo <- matrix(as.matrix(meanVectors[1,]), nrow = 28, ncol = 28, byrow = T) #mean two (or six)
png('meantwoPCA.png')
image(meanTwo[1:28,28:1])
dev.off()

meanSix <- matrix(as.matrix(meanVectors[2,]), nrow = 28, ncol = 28, byrow = T) #mean six (or two)
png('meansixPCA.png')
image(meanSix[1:28,28:1])
dev.off()

covmat <- round(cov(meanVectors),2)
melted_covmat <- melt(covmat)
g <- ggplot(data = melted_covmat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="PCA Method Covariance Plot")
png('covPCA.png')
g
dev.off()

#Creates confusion matrix
label <- as.data.frame(savePCA[[1]])
label$Prediction <- apply(label[,1:2],1,which.max)
label$Prediction[label$Prediction==2] <- 6 #These labels might need to switch depending on iteration.
label$Prediction[label$Prediction==1] <- 2
label$True <- labelTrue
table(label$Prediction,label$True)
prop.table(table(label$Prediction,label$True))

#Kmeans
kmean <- kmeans(data, 2)
label <- as.data.frame(kmean$cluster)
label$`kmean$cluster`[label$`kmean$cluster`==2] <- 6 #These labels might need to switch depending on iteration.
label$`kmean$cluster`[label$`kmean$cluster`==1] <- 2
label$True <- labelTrue
table(label$True,label$`kmean$cluster`)
prop.table(table(label$`kmean$cluster`,label$True))

#Plots log likelihood over iterations
logl <- as.data.frame(savePCA[[4]])
g <- ggplot(logl, aes(x=Iteration, y=logLikelihood)) + 
  geom_point() + 
  geom_line() +
  labs(title="PCA Method Log Likelihood Plot", 
       subtitle="Plot of Log Likelihood per Iteration",
       x="Iteration",
       y="Log Likelihood")
png('loglikelihoodPCA.png')
g
dev.off()


##GMM LRA Case
k = 2 #Number of Gaussians
r = 5 #Select dimensionality for E step. For low rank approx.
data <- dataProcessed

#GMM Use
saveLRA <- myGMM(data,k,r)

#Fitted Gaussians
componentWeights <- as.data.frame(saveLRA[[2]]) #weights
meanVectors <- as.data.frame(saveLRA[[3]])
meanTwo <- matrix(as.matrix(meanVectors[2,]), nrow = 28, ncol = 28, byrow = T) #mean two (or six)
png('meantwoLRA.png')
image(meanTwo[1:28,28:1])
dev.off()

meanSix <- matrix(as.matrix(meanVectors[1,]), nrow = 28, ncol = 28, byrow = T) #mean six (or two)
png('meansixLRA.png')
image(meanSix[1:28,28:1])
dev.off()

covmat <- round(cov(meanVectors),2)
melted_covmat <- melt(covmat)
g <- ggplot(data = melted_covmat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="Low Rank Approx. Method Covariance Plot")
png('covLRA.png')
g
dev.off()

#Creates confusion matrix
label <- as.data.frame(saveLRA[[1]])
label$Prediction <- apply(label[,1:2],1,which.max)
label$Prediction[label$Prediction==2] <- 6 #These labels might need to switch depending on iteration.
label$Prediction[label$Prediction==1] <- 2
label$True <- labelTrue
table(label$Prediction,label$True)
prop.table(table(label$Prediction,label$True))

#Plots log likelihood over iterations
logl <- as.data.frame(saveLRA[[4]])
g <- ggplot(logl, aes(x=Iteration, y=logLikelihood)) + 
  geom_point() + 
  geom_line() +
  labs(title="Low Rank Approx. Method Log Likelihood Plot", 
       subtitle="Plot of Log Likelihood per Iteration",
       x="Iteration",
       y="Log Likelihood")
png('loglikelihoodLRA.png')
g
dev.off()