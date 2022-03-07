##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(tictoc) #Not a base package. Make sure to install.
library(imager) #Not a base package. Make sure to install.


##Image Processing.
#This section imports an image using the imager package, breaks the image into the red, green, and blue channels,
#and then creates a dataframe with each column representing the color channels respectively.
#imageRaw <- load.image('beach.bmp') #Beach Image
imageRaw <- load.image('football.bmp') #Football Image

imageChannel <- channels(imageRaw)
imageChannel1 <- as.data.frame(imageChannel$c.1) #Red
imageChannel2 <- as.data.frame(imageChannel$c.2) #Green
imageChannel3 <- as.data.frame(imageChannel$c.3) #Blue
image <- cbind(imageChannel1$value,imageChannel2$value,imageChannel3$value)
image <- image * 255 #Scales values from 0-1 to 0-255.
image <- as.data.frame(image)
colnames(image) <- c('Red','Green','Blue')


##INPUT (Load the functions below before calling these.)
pixels <- image #Image used for the clustering.
k <- 15 #Number of initial clusters to try. The algorithm will reduce if it produces empty centroids.

mykMeans(pixels,k)
output <- mykMeans(pixels,k)
class <- as.data.frame(output[1])
centroid <- as.data.frame(output[2])


mykMedoids(pixels,k)
output <- mykMedoids(pixels,k)
class <- as.data.frame(output[1])
centroid <- as.data.frame(output[2])


#KMEANS FUNCTION
mykMeans <- function(pixels,k){
  tic()  
  iteration <- 0
  temp <- unique(pixels)
  if (nrow(temp) < k) {
    print('K is larger than the number of unique data points. Lowering k.')
    k <- nrow(pixels) 
  }
  
  #Initializes centroids
  centroid <- data.frame(Red = runif(1*k,min=0,max=255),
                         Green = runif(1*k,min=0,max=255),
                         Blue = runif(1*k,min=0,max=255))
  
  #Initializes class dataframe
  class <- data.frame(Assignment = runif(nrow(pixels*1),min=0,max=0))
  
  #Initialize distance matrix
  distance <- matrix(nrow = nrow(pixels), ncol = k)
  
  #Initialize change matrix used to track if changes occur during an iteration
  change <- matrix(1, nrow = k, ncol = ncol(pixels))
  
  
  while (mean(change) !=0){
    #Calculates distances from each datapoint to each centroid
    for (i in 1:k){
      temp <- sweep(as.matrix(pixels), 2, as.matrix(centroid[i,])) #Subtracts a given centroid from each row of a matrix
      temp2 <- rowSums(temp^2) #Squares and sums rows of matrix
      distance[,i] <- sqrt(temp2) 
    }
    
    #Updates class assignments
    class$Assignment <- apply(distance,1,which.min) #Assigns cluster based on minimum distance
    
    #Updates centroids
    for (i in 1:k){
      temp <- pixels[class$Assignment==i,] #Selects datapoints for a given cluster
      temp2 <- colMeans(temp)
      change[i,] <- rowSums(centroid[i,] - temp2) #Tracks changes
      centroid[i,] <- temp2
    }
    change <- change[complete.cases(change),] #Removes empty rows
    centroid <- centroid[complete.cases(centroid),]  #Removes empty cluster rows
    k <- nrow(centroid) #Updates number of clusters if needed
    iteration <- iteration + 1
    print(iteration)
  }
  output <- list(class,centroid)
  return(output)
  duration <- toc()
  duration <- duration$toc-duration$tic
  sprintf('Final number of clusters is %s and the algorithm was completed in %s iterations (%s seconds).', k, iteration,round(duration,2))
}


#KMEDOIDS FUNCTION
mykMedoids <- function(pixels,k){
  tic()
  iteration <- 0
  temp <- unique(pixels)
  if (nrow(temp) < k) {
    print('K is larger than the number of unique data points. Lowering k.')
    k <- nrow(pixels) 
  }
  
  #Initialize centroids
  pixelsU <- unique(pixels)
  centroid <- pixelsU[sample(nrow(pixelsU), k, replace=FALSE), ]
  
  #Initialize class dataframe 
  class <- data.frame(Assignment = runif(nrow(pixels*1),min=0,max=0))
  
  #Initialize distance matrix
  distance <- matrix(nrow = nrow(pixels), ncol = k)
  
  
  while (iteration <= 1000){
    
    #For a given centroid i, calculate the distances between the centroids and the data
    for (i in 1:k){
      temp <- sweep(as.matrix(pixels), 2, as.matrix(centroid[i,]))
      temp2 <- rowSums(temp^2)
      distance[,i] <- sqrt(temp2)
    }
    
    #Updates class assignment based on minimum distance
    class$Assignment <- apply(distance,1,which.min)
    
    #Updates centroids
    oldCentroid <- centroid
    newCentroid <- centroid
    for (i in 1:k){
      temp <- pixels[class$Assignment==i,] #Selects data in given centroid i 
      temp2 <- as.matrix(dist(temp)) #Pairwise distance matrix
      temp3 <- colMeans(temp2)
      temp4 <- which.min(temp3) #Location of minimum
      newCentroid[i,] <- temp[temp4,]
      centroid[i,] <- newCentroid[i,]
    }
    rm(temp5)
    centroid <- centroid[complete.cases(centroid),] #Removes empty centroids 
    k <- nrow(centroid) #Updates number of clusters
    iteration <- iteration + 1
    print(iteration)
    difference <- oldCentroid - newCentroid
    if (all(difference == 0)) {
      break
    }
  }
  output <- list(class,centroid)
  return(output)
  duration <- toc()
  duration <- duration$toc-duration$tic
  sprintf('Final number of clusters is %s and the algorithm was completed in %s iterations (%s seconds).', k, iteration, round(duration,2))
}