##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(lme4)
library(sjstats) #For ICC.
library(ggplot2)
library(misty) #Has handy centering function.
library(MuMIn)


##Data Input
reviewsRaw <- read.csv("ReviewsLabel.csv", header = T)


##Manipulation
reviews <- reviewsRaw
levels(reviews$semester)
reviews$semester <- factor(reviews$semester, levels = c("Summer 2014", "Fall 2014", "Spring 2015", "Summer 2015",
                                                        "Fall 2015", "Spring 2016", "Summer 2016",
                                                        "Fall 2016", "Spring 2017", "Summer 2017",
                                                        "Fall 2017", "Spring 2018", "Summer 2018",
                                                        "Fall 2018", "Spring 2019", "Summer 2019",
                                                        "Fall 2019", "Spring 2020", "Summer 2020"))
levels(reviews$difficulty)
reviews$difficulty <- factor(reviews$difficulty, levels = c("Very Easy", "Easy", "Medium", "Hard", "Very Hard"))
reviews$difficulty <- as.numeric(reviews$difficulty)
levels(reviews$rating)
reviews$rating <- factor(reviews$rating, levels = c("Strongly Disliked", "Disliked", "Neutral", "Liked", "Strongly Liked"))
reviews$rating <- as.numeric(reviews$rating)

reviews$hoursC <- center(reviews$hours, type = 'CWC', group = reviews$course) #Centers within cluster.
reviews$difficultyC <- center(reviews$difficulty, type = 'CWC', group = reviews$course)
reviews$ratingC <- center(as.numeric(reviews$rating), type = 'CWC', group = reviews$course)
reviews$semesterT <- as.numeric(reviews$semester) - 1
reviews$semesterTC <- center(reviews$semesterT, type = 'CWC', group = reviews$course)
reviews$negative <- ifelse(reviews$Rating == "negative", 1, 0) 


##Plots
agg <- aggregate(reviews[,5:7], list(reviews$semesterT), mean)
ggplot(agg, aes(x=Group.1, y=hours)) + geom_point() + 
  labs(y="Hours (per week)", 
       x="Semester", 
       title="Scatterplot of Average Hours (per week) per Semester")
ggplot(agg, aes(x=Group.1, y=difficulty)) + geom_point() + 
  labs(y="Difficulty", 
       x="Semester", 
       title="Scatterplot of Average Difficulty per Semester")
ggplot(agg, aes(x=Group.1, y=rating)) + geom_point() + 
  labs(y="Rating", 
       x="Semester", 
       title="Scatterplot of Average Rating per Semester")

agg2 <- aggregate(reviews[,13], list(reviews$semesterT), mean)
ggplot(agg2, aes(x=Group.1, y=x)) + geom_point() + 
  labs(y="Proportion Negative", 
       x="Semester", 
       title="Proportion of Negative Reviews per Semester")

agg3 <- as.data.frame(table(reviews$semesterT))
ggplot(agg3, aes(x=Var1, y=Freq)) + geom_point() + 
  labs(y="Count", 
       x="Semester", 
       title="Scatterplot of Reviews per Semester")


##Models
#Model 1
reviews.lr1 <- glmer(negative ~ (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr1)
performance::r2(reviews.lr1)

ICC <- performance::icc(reviews.lr1)
DesEff <- 1 + ICC$ICC_adjusted*((nrow(reviews)/(length(unique(reviews$course))))-1)
ICC; DesEff

#Model 2
reviews.lr2 <- glmer(negative ~ hoursC + difficultyC + ratingC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr2)
performance::r2(reviews.lr2)

#Model 3
reviews.lr3 <- glmer(negative ~ hoursC + difficultyC + ratingC + semesterTC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr3)
performance::r2(reviews.lr3)

#Model 4
reviews.lr4 <- glmer(negative ~ hoursC + difficultyC + ratingC + semesterTC + hoursC:semesterTC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr4)
performance::r2(reviews.lr4)

#Model 5
reviews.lr5 <- glmer(negative ~ hoursC + difficultyC + ratingC + semesterTC + difficultyC:semesterTC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr5)
performance::r2(reviews.lr5)

#Model 6
reviews.lr6 <- glmer(negative ~ hoursC + difficultyC + ratingC + semesterTC + ratingC:semesterTC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr6)
performance::r2(reviews.lr6)

##Robust Check
reviews.lr21 <- glmer(negative ~ hoursC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr21)
performance::r2(reviews.lr21)

reviews.lr22 <- glmer(negative ~ hoursC + difficultyC + (1 | course),
                     data = reviews,
                     family = binomial(link = 'logit'))
summary(reviews.lr22)
performance::r2(reviews.lr22)

