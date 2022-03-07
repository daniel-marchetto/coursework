##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)

##Libraries
library(R.matlab)
library(igraph)


##DATA IMPORT
isomapRaw <- readMat('isomap.mat')
temp <- lapply(isomapRaw, unlist, use.names=FALSE)
temp2 <- as.matrix(cbind(temp$images)) 
isomap <- t(temp2)


##EUCLIDEAN DISTANCE MATRIX
distance <- as.matrix(dist(isomap, method = 'euclidean'))


##EPSILON TUNING
epsilon <- 19.81
temp <- distance
temp[temp < epsilon] <- 0
temp[temp >= epsilon] <- 1
counts <- rowSums(temp)
min(counts)
max(counts)


##VISUALIZATION
similarity <- distance
similarity[similarity < epsilon] <- 0
int <- sample.int(698,60,replace = FALSE)
reduce <- similarity[int,int]
g1 <- graph.adjacency(reduce, weighted = TRUE)
g2 <- delete.vertices(g1, degree(g1)<1)
png("euclideanGraph.png", width = 15, height = 15, units = "in", res = 500)
plot.igraph(g2,vertex.size=10, 
            vertex.label.cex=1, 
            layout=layout.fruchterman.reingold(g2, niter=1000)) 
dev.off()

##ISOMAP IMPLEMENTATION
#Build weighted adjacency matrix
similarity <- distance
similarity[similarity < epsilon] <- 0

#Computer pairwise shortest distances
pwsdistance <- shortest.paths(graph.adjacency(similarity, weighted = TRUE))
pwsdistance2 <- pwsdistance^2

#Center the matrix
H <- diag(nrow(similarity)) - matrix(1/nrow(similarity),nrow = nrow(similarity), ncol = nrow(similarity))
C <- (-1/(2*nrow(similarity))) * (H %*% pwsdistance2 %*% H)

#Eigendecomposition to reduce dimensionality
decomposition <- eigen(C)
lambda <- sqrt(decomposition$values[1:2])
vec <- decomposition$vectors[,1:2]
transformed <- vec %*% diag(lambda)
png('euclidt.png')
plot(transformed, main = 'Euclidean Distance ISOMAP')
dev.off()

#Images
identify(transformed[,1],transformed[,2], plot=TRUE)
com1 <- matrix(isomap[401,],64,64)
png('401.png')
image(com1)
dev.off()
com2 <- matrix(isomap[686,],64,64)
png('686.png')
image(com2)
dev.off()
com3 <- matrix(isomap[675,],64,64)
png('675.png')
image(com3)
dev.off()

##MANHATTAN DISTANCE MATRIX
distance <- as.matrix(dist(isomap, method = 'manhattan'))


##EPSILON TUNING
epsilon <- 964
temp <- distance
temp[temp < epsilon] <- 0
temp[temp >= epsilon] <- 1
counts <- rowSums(temp)
min(counts)
max(counts)


##VISUALIZATION
similarity <- distance
similarity[similarity < epsilon] <- 0
int <- sample.int(698,60,replace = FALSE)
reduce <- similarity[int,int]
g1 <- graph.adjacency(reduce, weighted = TRUE)
g2 <- delete.vertices(g1, degree(g1)<1)
png("manhattanGraph.png", width = 15, height = 15, units = "in", res = 500)
plot.igraph(g2,vertex.size=10, 
            vertex.label.cex=1, 
            layout=layout.fruchterman.reingold(g2, niter=1000)) 
dev.off()

##ISOMAP IMPLEMENTATION
#Build weighted adjacency matrix
similarity <- distance
similarity[similarity < epsilon] <- 0

#Computer pairwise shortest distances
pwsdistance <- shortest.paths(graph.adjacency(similarity, weighted = TRUE))
pwsdistance2 <- pwsdistance^2

#Center the matrix
H <- diag(nrow(similarity)) - matrix(1/nrow(similarity),nrow = nrow(similarity), ncol = nrow(similarity))
C <- (-1/(2*nrow(similarity))) * (H %*% pwsdistance2 %*% H)

#Eigendecomposition to reduce dimensionality
decomposition <- eigen(C)
lambda <- sqrt(decomposition$values[1:2])
vec <- decomposition$vectors[,1:2]
transformed <- vec %*% diag(lambda)
png('manhat.png')
plot(transformed, main = 'Manhattan Distance ISOMAP')
dev.off()

#Images
identify(transformed[,1],transformed[,2], plot=TRUE)
com1 <- matrix(isomap[266,],64,64)
png('266.png')
image(com1)
dev.off()
com2 <- matrix(isomap[449,],64,64)
png('449.png')
image(com2)
dev.off()
com3 <- matrix(isomap[136,],64,64)
png('136.png')
image(com3)
dev.off()
