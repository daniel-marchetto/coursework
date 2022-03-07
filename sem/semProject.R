##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)


##Libraries
library(psych)
library(lavaan)
library(semPlot)
library(semTools)
library(ggplot2)
library(ggcorrplot)


##Data Loading
raw.df <- read.csv('Responses.csv', header = T)

demographics <- cbind(raw.df[,141:160])
interests <- cbind(raw.df[,32:63])
personality <- cbind(raw.df[,77:133])


personality2 <- na.omit(personality)
interests2 <- na.omit(interests)


###Preliminary Data Analysis
icorr <- round(cor(interests2),1)

ggcorrplot(icorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("blue", "white", "orange"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

pcorr <- round(cor(personality2),1)

ggcorrplot(pcorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("blue", "white", "orange"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

alpha(personality2, check.keys = T)
alpha(interests2, check.keys = T)


###Preliminary Factor Analysis
##Personality
scree(personality2, factors = FALSE, pc =  TRUE)
scree(personality2, factors = TRUE, pc =  FALSE)

explore4 <- fa(personality2, nfactors = 4, rotate = 'promax')
print(explore4)
plot(explore4)
fa.diagram(explore4)
loading <- loadings(explore4)
df <- as.data.frame(loading[1:57,1:4])
df2 <- df
df2[abs(df)<0.35] <- NA
write.csv(df2, file = 'personality4.csv')

explore5 <- fa(personality2, nfactors = 5, rotate = 'promax')
print(explore5)
plot(explore5)
fa.diagram(explore5)
loading <- loadings(explore5)
df <- as.data.frame(loading[1:57,1:5])
df2 <- df
df2[abs(df)<0.35] <- NA
write.csv(df2, file = 'personality5.csv')

explore6 <- fa(personality2, nfactors = 6, rotate = 'promax')
print(explore6)
plot(explore6)
fa.diagram(explore6)
loading <- loadings(explore6)
df <- as.data.frame(loading[1:57,1:6])
df2 <- df
df2[abs(df)<0.35] <- NA
write.csv(df2, file = 'personality6.csv')

explore7 <- fa(personality2, nfactors = 7, rotate = 'promax')
print(explore7)
plot(explore7)
fa.diagram(explore7)
loading <- loadings(explore7)
df <- as.data.frame(loading[1:57,1:7])
df2 <- df
df2[abs(df)<0.35] <- NA
write.csv(df2, file = 'personality7.csv')


##Confirmatory Factor Analysis
cfa.model1 <- '
pF1 =~ Loneliness + Changing.the.past + Mood.swings + Self.criticism + Energy.levels + Personality + Happiness.in.life
pF2 =~ Prioritising.workload + Workaholism + Writing.notes + Cheating.in.school + Getting.up 
pF3 =~ Getting.angry + Health + Knowing.the.right.people + Appearence.and.gestures + Waiting
pF4 =~ Life.struggles + Empathy + Compassion.to.animals + Giving + Children + Fake
pF5 =~ God + Final.judgement + Charity 
pF6 =~ Reliability + Thinking.ahead + Keeping.promises + Borrowed.stuff
pF7 =~ Socializing + New.environment + Interests.or.hobbies + Assertiveness + Number.of.friends + Decision.making + Public.speaking + Energy.levels
'

cfa.fit1 <- cfa(cfa.model1, data = personality)
summary(cfa.fit1, fit.measures = TRUE)

cfa.model2 <- '
pF1 =~ Loneliness + Changing.the.past + Mood.swings + Self.criticism + Energy.levels + Personality + Happiness.in.life
pF2 =~ Prioritising.workload + Workaholism + Writing.notes + Cheating.in.school + Getting.up + Decision.making 
pF3 =~ Getting.angry + Health + Knowing.the.right.people + Appearence.and.gestures + Waiting
pF4 =~ Life.struggles + Empathy + Compassion.to.animals + Giving + Children + Fake
pF5 =~ God + Final.judgement + Charity + Giving
pF6 =~ Reliability + Thinking.ahead + Keeping.promises + Borrowed.stuff
pF7 =~ Socializing + New.environment + Interests.or.hobbies + Assertiveness + Number.of.friends + Decision.making + Public.speaking + Energy.levels
'

cfa.fit2 <- cfa(cfa.model2, data = personality)
summary(cfa.fit2, fit.measures = TRUE)
semPaths(cfa.fit2, residuals = F)


##Interests
scree(interests2, factors = TRUE, pc =  FALSE)

explore4 <- fa(interests2, nfactors = 4, rotate = 'promax')
print(explore4)
plot(explore4)
fa.diagram(explore4)

explore5 <- fa(interests2, nfactors = 5, rotate = 'promax')
print(explore5)
plot(explore5)
fa.diagram(explore5)

explore6 <- fa(interests2, nfactors = 6, rotate = 'promax')
print(explore6)
plot(explore6)
fa.diagram(explore6)


##Confirmatory Factor Analysis
cfa.model1 <- '
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Dancing + Foreign.languages + Psychology + History + Politics + Law + Geography + Economy.Management
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics
iF3 =~ Biology + Chemistry + Medicine 
iF4 =~ Shopping + Celebrities + Dancing
iF6 =~ Cars + Adrenaline.sports + Active.sport + Passive.sport
'

cfa.fit1 <- cfa(cfa.model1, data = interests)
summary(cfa.fit1, fit.measures = TRUE)

cfa.model2 <- '
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Dancing + Foreign.languages + Psychology + History + Politics + Law + Geography
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics
iF4 =~ Shopping + Celebrities + Dancing
iF5 =~ Cars + Adrenaline.sports + Active.sport + Passive.sport
'

cfa.fit2 <- cfa(cfa.model2, data = interests)
summary(cfa.fit2, fit.measures = TRUE)

cfa.model3 <- '
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Dancing + Foreign.languages + Psychology + History + Politics + Law + Geography
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics
'

cfa.fit3 <- cfa(cfa.model3, data = interests)
summary(cfa.fit3, fit.measures = TRUE)


##CFA MODEL
library(lavaan)
cfa.model4 <- '
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Foreign.languages + History + Law
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics + Psychology
iF4 =~ Foreign.languages  + History + Politics + Law + Geography  + Economy.Management + Psychology
'

cfa.fit4 <- cfa(cfa.model4, data = interests)
summary(cfa.fit4, fit.measures = TRUE)
semPaths(cfa.fit4, residuals = F)


##Structural Equation Model
sem.model1 <- '
#measurement model
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Foreign.languages + History + Law
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics + Psychology
iF4 =~ Politics + Foreign.languages  + History + Law + Geography  + Economy.Management + Psychology
pF1 =~ Loneliness + Changing.the.past + Mood.swings + Self.criticism + Energy.levels + Personality + Happiness.in.life
pF2 =~ Prioritising.workload + Workaholism + Writing.notes + Cheating.in.school + Getting.up + Decision.making 
pF3 =~ Getting.angry + Health + Knowing.the.right.people + Appearence.and.gestures + Waiting
pF4 =~ Life.struggles + Empathy + Compassion.to.animals + Giving + Children + Fake
pF5 =~ God + Final.judgement + Charity + Giving
pF6 =~ Reliability + Thinking.ahead + Keeping.promises + Borrowed.stuff
pF7 =~ Socializing + New.environment + Interests.or.hobbies + Assertiveness + Number.of.friends + Decision.making + Public.speaking + Energy.levels


#path models
iF1 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF2 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF3 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF4 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7

#error correlations


'

sem.fit1 <- sem(sem.model1, data = raw.df)
summary(sem.fit1, fit.measures = TRUE, standardized = TRUE)
semPaths(sem.fit1, residuals = F)


##SEM MODEL
library(lavaan)
sem.model2 <- '
#measurement model
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Foreign.languages + History + Law
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics + Psychology
iF4 =~ Politics + Foreign.languages  + History + Law + Geography  + Economy.Management + Psychology
pF1 =~ Loneliness + Changing.the.past + Mood.swings + Self.criticism + Energy.levels + Personality + Happiness.in.life
pF2 =~ Prioritising.workload + Workaholism + Writing.notes + Cheating.in.school + Getting.up + Decision.making 
pF3 =~ Getting.angry + Health + Knowing.the.right.people + Appearence.and.gestures + Waiting
pF4 =~ Life.struggles + Empathy + Compassion.to.animals + Giving + Children + Fake
pF5 =~ God + Final.judgement + Charity + Giving
pF6 =~ Reliability + Thinking.ahead + Keeping.promises + Borrowed.stuff
pF7 =~ Socializing + New.environment + Interests.or.hobbies + Assertiveness + Number.of.friends + Decision.making + Public.speaking + Energy.levels


#path models
iF1 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF2 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF3 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF4 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7

#error correlations
Biology ~~ Medicine
PC ~~ Internet
Politics ~~ Law
iF1 ~~ iF4
iF2 ~~ iF3
pF1 ~~ pF4
pF2 ~~ pF6

'

sem.fit2 <- sem(sem.model2, data = raw.df, orthogonal = TRUE)
summary(sem.fit2, fit.measures = TRUE, standardized = TRUE)


##SEM DIAGRAM
library(semPlot)
semPaths(sem.fit2, residuals = F)

sem.model3 <- '
#measurement model
iF1 =~ Art.exhibitions + Theatre + Writing + Musical.instruments + Reading + Religion + Foreign.languages + History + Law
iF2 =~ PC + Science.and.technology + Mathematics + Internet + Physics + Economy.Management
iF3 =~ Biology + Chemistry + Medicine + Physics + Psychology
iF4 =~ Politics + Foreign.languages  + History + Law + Geography  + Economy.Management + Psychology
pF1 =~ Loneliness + Changing.the.past + Mood.swings + Self.criticism + Energy.levels + Personality + Happiness.in.life
pF2 =~ Prioritising.workload + Workaholism + Writing.notes + Cheating.in.school + Getting.up + Decision.making 
pF3 =~ Getting.angry + Health + Knowing.the.right.people + Appearence.and.gestures + Waiting
pF4 =~ Life.struggles + Empathy + Compassion.to.animals + Giving + Children + Fake
pF5 =~ God + Final.judgement + Charity + Giving
pF6 =~ Reliability + Thinking.ahead + Keeping.promises + Borrowed.stuff
pF7 =~ Socializing + New.environment + Interests.or.hobbies + Assertiveness + Number.of.friends + Decision.making + Public.speaking + Energy.levels


#path models
iF1 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF2 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF3 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
iF4 ~ pF1 + pF2 + pF3 + pF4 + pF5 + pF6 + pF7
pF2 ~ pF1 + pF4 + pF7
pF6 ~ pF1 + pF7
pF7 ~ pF1 
pF3 ~ pF1

#error correlations
Biology ~~ Medicine
PC ~~ Internet
Politics ~~ Law

'

sem.fit3 <- sem(sem.model3, data = raw.df)
summary(sem.fit3, fit.measures = TRUE, standardized = TRUE)

semPaths(sem.fit3)


##GROUP COMPARISONS
library(semTools)
measurementInvariance(model = cfa.model4, data = raw.df, group = 'Gender')

sem.fit4 <- sem(sem.model2, data = raw.df, group = 'Gender', group.equal = c('loadings','intercepts'))
summary(sem.fit4, fit.measures = TRUE, standardized = TRUE)

measurementInvariance(model = cfa.model4, data = raw.df, group = 'Left...right.handed')

sem.fit4 <- sem(sem.model2, data = raw.df, group = 'Left...right.handed', group.equal = c('loadings','intercepts'))
summary(sem.fit4, fit.measures = TRUE, standardized = TRUE)

