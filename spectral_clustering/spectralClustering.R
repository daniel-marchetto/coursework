##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(igraph)
library(caret)


##Data Processing
nodes <- read.delim("nodes.txt", header=FALSE)
edges <- read.delim("edges.txt", header=FALSE)


##Spectral Clustering Algorithm
#Creation of Adjacency Matrix
adjacency <- get.adjacency(graph.edgelist(as.matrix(edges), directed = F))

#Creation of Diagonal Matrix
diaV <- as.vector(rowSums(as.matrix(adjacency)))
degree <- diag(diaV)

#Creation of Laplace Matrix
laplace <- degree - adjacency
laplaceNorm <- sweep(as.matrix(laplace), 2, colSums(as.matrix(laplace)), FUN="/")

#Calculation of Eigvenvectors
proper <- eigen(laplace)
#proper <- eigen(laplaceNorm) #Test to see if normalization affects accuracy. It does not seem to.

#Kmean assignment of cluster
properk <- proper$vectors[,1222:1490]

properk.cluster <- kmeans(properk, 2)

#Comparison to 'Ground Truth' Labels
class <- data.frame(V1 = 1:1490,
                    Label = properk.cluster$cluster - 1)

comparison <- merge(nodes,class, by = 'V1')

cm <- confusionMatrix(as.factor(comparison$Label),as.factor(comparison$V3))
cm #Accuracy results


