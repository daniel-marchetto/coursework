##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraies
library(data.table)
library(softImpute)
library(dplyr)
library(xlsx)


##File Import and Data Prep
movies <- read.csv("movie.csv", header = T)
movieList <- c("The Shawshank Redemption","The Godfather","The Dark Knight",	
               "The Godfather Part II","The Lord of the Rings III","Pulp Fiction",	
               "Schindlers List","The Good, the Bad, and the Ugly","12 Angry Men",	
               "Inception","Fight Club","The Lord of the Rings I",	
               "Forrest Gump","Star Wars V the Empire Strikes Back","The Lord of the Rings II",	
               "The Matrix","Goodfellas","One Flew over the Cuckoos Nest",
               "Seven Samurai","Interstellar")
colnames(movies)[15:34] <- movieList

movies  <- subset(movies, select = movieList)

moviesT <- transpose(movies)
colnames(moviesT) <- rownames(movies)
rownames(moviesT) <- colnames(movies)


##Simularity Measure and Functions
sim <- function(u,v,d){
  if (d == "l2"){
    distance <- sqrt(sum((u - v) ^ 2))
  }
  if (d == "l1"){
    distance <- sum( abs(u - v) )
  }
  if (d == "l0"){
    distance <- sum(u != v)
  }
  return(exp(-(distance^2)))
}

siml2 <- function(u,v){
  distance <- sqrt(sum((u - v) ^ 2))
  return(exp(-(distance^2)))
}

siml1 <- function(u,v){
  distance <- sum( abs(u - v) )
  return(exp(-(distance^2)))
}

siml0 <- function(u,v){
  distance <- sum(u != v)
  return(exp(-(distance^2)))
}

matab<-Vectorize(
  function(a,b,fun,data) {
    fun(data[a,],data[b,])
  }, vectorize.args=list("a","b")
)


##User Based Recommender System
UBsim_l2 <- outer(1:nrow(movies),1:nrow(movies),matab,fun=siml2,data=as.matrix(movies))
UBsim_l1 <- outer(1:nrow(movies),1:nrow(movies),matab,fun=siml1,data=as.matrix(movies))
UBsim_l0 <- outer(1:nrow(movies),1:nrow(movies),matab,fun=siml0,data=as.matrix(movies))

simUB <- UBsim_l2
moviesUB <- movies
for (i in 1:ncol(movies)){
  for (j in 1:nrow(movies)){
    if (movies[j,i] == 0){
      temp <- data.frame(movies[,i], simUB[,j], movies[,i]*simUB[,j])
      temp2 <- subset(temp, temp[,1] != 0)
      replacement <- sum(temp2[,3]) / sum(temp2[,2])
      moviesUB[j,i] <- replacement
    }
  }
}


userRec <- data.frame('recommended movie 1' = numeric(),
                      'recommended movie 2' = numeric(),
                      'recommended movie 3' = numeric(),
                      'recommended movie 4' = numeric(),
                      'recommended movie 5' = numeric())

for (i in 1:nrow(moviesUB)){
  userRec[i,] <- colnames(head(sort(-moviesUB[i,]), 5))[1:5]
}

userRec2 <- data.frame('score of the recommendedmovie recommended movie 1' = numeric(),
                       'score of the recommendedmovie recommended movie 2' = numeric(),
                       'score of the recommendedmovie recommended movie 3' = numeric(),
                       'score of the recommendedmovie recommended movie 4' = numeric(),
                       'score of the recommendedmovie recommended movie 5' = numeric())

for (i in 1:nrow(moviesUB)){
  userRec2[i,] <- head(sort(-moviesUB[i,]), 5)[1:5]
}

usersheet <- data.frame(name = numeric(nrow(movies)),
                        userRec$recommended.movie.1,userRec2$score.of.the.recommendedmovie.recommended.movie.1,
                        userRec$recommended.movie.2,userRec2$score.of.the.recommendedmovie.recommended.movie.2,
                        userRec$recommended.movie.3,userRec2$score.of.the.recommendedmovie.recommended.movie.3,
                        userRec$recommended.movie.4,userRec2$score.of.the.recommendedmovie.recommended.movie.4,
                        userRec$recommended.movie.5,userRec2$score.of.the.recommendedmovie.recommended.movie.5)
usersheet$name <- rownames(usersheet)


##Item Based Recommender System
IBsim_l2 <- outer(1:nrow(moviesT),1:nrow(moviesT),matab,fun=siml2,data=as.matrix(moviesT))
IBsim_l1 <- outer(1:nrow(moviesT),1:nrow(moviesT),matab,fun=siml1,data=as.matrix(moviesT))
IBsim_l0 <- outer(1:nrow(moviesT),1:nrow(moviesT),matab,fun=siml0,data=as.matrix(moviesT))

simIB <- IBsim_l2
moviesIB <- moviesT
for (i in 1:ncol(moviesT)){
  for (j in 1:nrow(moviesT)){
    if (moviesT[j,i] == 0){
      temp <- data.frame(moviesT[,i], simIB[,j], moviesT[,i]*simIB[,j])
      temp2 <- subset(temp, temp[,1] != 0)
      replacement <- sum(temp2[,3]) / sum(temp2[,2])
      moviesIB[j,i] <- replacement
    }
  }
}

moviesIBT <- as.data.frame(t(as.matrix(moviesIB)))

itemRec <- data.frame('recommended movie 1' = numeric(),
                      'recommended movie 2' = numeric(),
                      'recommended movie 3' = numeric(),
                      'recommended movie 4' = numeric(),
                      'recommended movie 5' = numeric())

for (i in 1:nrow(moviesIBT)){
  itemRec[i,] <- colnames(head(sort(-moviesIBT[i,]), 5))[1:5]
}

itemRec2 <- data.frame('score of the recommendedmovie recommended movie 1' = numeric(),
                       'score of the recommendedmovie recommended movie 2' = numeric(),
                       'score of the recommendedmovie recommended movie 3' = numeric(),
                       'score of the recommendedmovie recommended movie 4' = numeric(),
                       'score of the recommendedmovie recommended movie 5' = numeric())

for (i in 1:nrow(moviesIBT)){
  itemRec2[i,] <- head(sort(-moviesIBT[i,]), 5)[1:5]
}

itemsheet <- data.frame(name = numeric(nrow(movies)),
                        itemRec$recommended.movie.1,itemRec2$score.of.the.recommendedmovie.recommended.movie.1,
                        itemRec$recommended.movie.2,itemRec2$score.of.the.recommendedmovie.recommended.movie.2,
                        itemRec$recommended.movie.3,itemRec2$score.of.the.recommendedmovie.recommended.movie.3,
                        itemRec$recommended.movie.4,itemRec2$score.of.the.recommendedmovie.recommended.movie.4,
                        itemRec$recommended.movie.5,itemRec2$score.of.the.recommendedmovie.recommended.movie.5)
itemsheet$name <- rownames(itemsheet)


##Soft Impute
moviesM <- as.matrix(movies)
moviesM[moviesM == 0] <- NA

movie.impute <- softImpute(moviesM)
moviesI <- as.data.frame(complete(moviesM, movie.impute))
colnames(moviesI) <- colnames(movies)
rownames(moviesI) <- rownames(movies)

imputeRec <- data.frame('recommended movie 1' = numeric(),
                        'recommended movie 2' = numeric(),
                        'recommended movie 3' = numeric(),
                        'recommended movie 4' = numeric(),
                        'recommended movie 5' = numeric())

for (i in 1:nrow(moviesI)){
  imputeRec[i,] <- colnames(head(sort(-moviesI[i,]), 5))[1:5]
}

imputeRec2 <- data.frame('score of the recommendedmovie recommended movie 1' = numeric(),
                         'score of the recommendedmovie recommended movie 2' = numeric(),
                         'score of the recommendedmovie recommended movie 3' = numeric(),
                         'score of the recommendedmovie recommended movie 4' = numeric(),
                         'score of the recommendedmovie recommended movie 5' = numeric())

for (i in 1:nrow(moviesI)){
  imputeRec2[i,] <- head(sort(-moviesI[i,]), 5)[1:5]
}

imputesheet <- data.frame(name = numeric(nrow(movies)),
                          imputeRec$recommended.movie.1,imputeRec2$score.of.the.recommendedmovie.recommended.movie.1,
                          imputeRec$recommended.movie.2,imputeRec2$score.of.the.recommendedmovie.recommended.movie.2,
                          imputeRec$recommended.movie.3,imputeRec2$score.of.the.recommendedmovie.recommended.movie.3,
                          imputeRec$recommended.movie.4,imputeRec2$score.of.the.recommendedmovie.recommended.movie.4,
                          imputeRec$recommended.movie.5,imputeRec2$score.of.the.recommendedmovie.recommended.movie.5)
imputesheet$name <- rownames(imputesheet)

##Saving
write.xlsx(usersheet, file = 'collab_filter.xlsx', sheet = 'user', row.names = F)
write.xlsx(itemsheet, file = 'collab_filter.xlsx', sheet = 'item', append = T, row.names = F)
write.xlsx(imputesheet, file = 'collab_filter.xlsx', sheet = 'softimpute', append = T, row.names = F)