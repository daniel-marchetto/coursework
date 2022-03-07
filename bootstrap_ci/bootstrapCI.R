##Set working directory.
directory = "YOUR DIRECTORY"
setwd(directory)

##Libraries.
library(ggplot2)
library(ggExtra)
library(survival)

##KM Median
lung <- lung

lung.km <- survfit(Surv(time, status) ~ 1, data = lung, type = 'kaplan-meier',
                   conf.int = 0.9, conf.type = 'plain') #KM estimate from part 1.

thetaHat <- quantile(lung.km, probs = 0.5)$quantile

KMtable <- data.frame(method = character(0),
                 lower = numeric(0),
                 upper = numeric(0),
                 stringsAsFactors = FALSE)

#Bootstap
KMbootstrap <- data.frame(medians = numeric())
for (i in 1:10000){
  temp <-  lung[sample(nrow(lung), replace = T), ] #Samples dataframe with replacement up to size of dataframe.
  temp.km <- survfit(Surv(time, status) ~ 1, data = temp, type = 'kaplan-meier')
  temp.quan <- quantile(temp.km, probs = 0.5) #Calculates median.
  KMbootstrap[i,1] <- temp.quan$quantile #Records median.
}
orderStat <- KMbootstrap[with(KMbootstrap, order(medians)),] #Sorts results.

thetaStar <- mean(KMbootstrap$medians)
seHStar <- sqrt(var(KMbootstrap$medians)) 

#Method I (Normal CI)
KMtable[1,1] <- 'I'
KMtable[1,2] <- thetaHat - (1.645 * seHStar)
KMtable[1,3] <- thetaHat + (1.645 * seHStar)

#Method II (R Large Sample CI)
KMtable[2,1] <- 'II'
KMtable[2,2] <- quantile(lung.km, probs = 0.5)$lower
KMtable[2,3] <- quantile(lung.km, probs = 0.5)$upper

#Method III (Percentile CI)
KMtable[3,1] <- 'III'
KMtable[3,2] <- orderStat[500]
KMtable[3,3] <- orderStat[9500]

gKM <- ggplot(as.data.frame(KMbootstrap), aes(medians)) + 
  geom_histogram() +
  labs(title="Bootstrapped KM Estimate Medians", 
       x="Median",
       y="Count")
gKM

#Method IV (Pivot CI)
KMtable[4,1] <- 'IV'
KMtable[4,2] <- (2*thetaHat) - orderStat[9500]
KMtable[4,3] <- (2*thetaHat) - orderStat[500]


#Method V (Student CI)
t <- (orderStat - thetaHat) / seHStar

KMtable[5,1] <- 'V'
KMtable[5,2] <- thetaHat - (t[9500]*seHStar)
KMtable[5,3] <- thetaHat - (t[500]*seHStar)

#Method VI (BCA)
p0 <- sum((KMbootstrap$medians - thetaHat) < 0) / nrow(KMbootstrap)
z0 <- qnorm(p0)
a0 <- sum((thetaStar - KMbootstrap$medians)^3) / (6 * (sum((thetaStar - KMbootstrap$medians)^2))^(3/2))
q1 <- pnorm(z0 + ((z0 + qnorm(0.05)) / (1 - a0*(z0 + qnorm(0.05)))))
q2 <- pnorm(z0 + ((z0 + qnorm(0.95)) / (1 - a0*(z0 + qnorm(0.95)))))

KMtable[6,1] <- 'VI'
KMtable[6,2] <- quantile(KMbootstrap$medians, probs = q1)
KMtable[6,3] <- quantile(KMbootstrap$medians, probs = q2)


#Method VII (HDP)
HDP <- data.frame(lower = numeric(),
                  upper = numeric(),
                  range = numeric())
for (i in 1:1000){
  HDP[i,1] <- orderStat[i]
  HDP[i,2] <- orderStat[i+9000]
  HDP[i,3] <- HDP[i,2] - HDP[i,1]
}

KMtable[7,1] <- 'VII'
KMtable[7,2] <- HDP[which.min(HDP$range),1]
KMtable[7,3] <- HDP[which.min(HDP$range),2]


##PH Coeficient
heartdata <- read.csv("heart_failure_clinical_records_dataset.csv", header = T)

heart.ph <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + sex #PH regression from part 1.
                  + high_blood_pressure + smoking + diabetes, data = heartdata)
summary(heart.ph)

thetaHat2 <- heart.ph$coefficients[1]

PHtable <- data.frame(method = character(0),
                      lower = numeric(0),
                      upper = numeric(0),
                      stringsAsFactors = FALSE)

#Bootstap
PHbootstrap <- data.frame(coeffs = numeric())
for (i in 1:10000){
  temp <-  heartdata[sample(nrow(heartdata), replace = T), ] #Samples dataframe with replacement up to size of dataframe.
  temp.ph <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + sex + high_blood_pressure + smoking + diabetes, data = temp)
  PHbootstrap[i,1] <- temp.ph$coefficients[1] #1 is age.
}
orderStatat2 <- PHbootstrap[with(PHbootstrap, order(coeffs)),] #Sorts results.

thetaStar2 <- mean(PHbootstrap$coeffs)
seHStar2 <- sqrt(var(PHbootstrap$coeffs))

#Method I (Normal CI)
PHtable[1,1] <- 'I'
PHtable[1,2] <- thetaHat2 - (1.645 * seHStar2)
PHtable[1,3] <- thetaHat2 + (1.645 * seHStar2)

#Method II (R Large Sample CI)

PHtable[2,1] <- 'II'
PHtable[2,2] <- confint(heart.ph, level = 0.9)[1]
PHtable[2,3] <- confint(heart.ph, level = 0.9)[7]

#Method III (Percentile CI)
PHtable[3,1] <- 'III'
PHtable[3,2] <- orderStatat2[500]
PHtable[3,3] <- orderStatat2[9500]

gPH <- ggplot(as.data.frame(PHbootstrap), aes(exp(coeffs))) + 
  geom_histogram() +
  labs(title="Bootstrapped exp(Age Coefficient) Estimates", 
       x="exp(Coefficient) Estimate",
       y="Count")
gPH


#Method IV (Pivot CI)
PHtable[4,1] <- 'IV'
PHtable[4,2] <- (2*thetaHat2) - orderStatat2[9500]
PHtable[4,3] <- (2*thetaHat2) - orderStatat2[500]


#Method V (Student CI)
t2 <- (orderStatat2 - thetaHat2) / seHStar2

PHtable[5,1] <- 'V'
PHtable[5,2] <- thetaHat2 - (t2[9500]*seHStar2)
PHtable[5,3] <- thetaHat2 - (t2[500]*seHStar2)

#Method VI (BCA)
p0 <- sum((PHbootstrap$coeffs - thetaHat2) < 0) / nrow(PHbootstrap)
z0 <- qnorm(p0)
a0 <- sum((thetaStar2 - PHbootstrap$coeffs)^3) / (6 * (sum((thetaStar2 - PHbootstrap$coeffs)^2))^(3/2))
q1 <- pnorm(z0 + ((z0 + qnorm(0.05)) / (1 - a0*(z0 + qnorm(0.05)))))
q2 <- pnorm(z0 + ((z0 + qnorm(0.95)) / (1 - a0*(z0 + qnorm(0.95)))))

PHtable[6,1] <- 'VI'
PHtable[6,2] <- quantile(PHbootstrap$coeffs, probs = q1)
PHtable[6,3] <- quantile(PHbootstrap$coeffs, probs = q2)


#Method VII (HDP)
HDP2 <- data.frame(lower = numeric(),
                  upper = numeric(),
                  range = numeric())
for (i in 1:1000){
  HDP2[i,1] <- orderStatat2[i]
  HDP2[i,2] <- orderStatat2[i+9000]
  HDP2[i,3] <- HDP2[i,2] - HDP2[i,1]
}

PHtable[7,1] <- 'VII'
PHtable[7,2] <- HDP2[which.min(HDP$range),1]
PHtable[7,3] <- HDP2[which.min(HDP$range),2]

#Transform
PHtable$lower <- exp(PHtable$lower)
PHtable$upper <- exp(PHtable$upper)

##Information Save
KMtable$length <- KMtable$upper - KMtable$lower
PHtable$length <- PHtable$upper - PHtable$lower
write.csv(KMtable, file = 'KMci.csv')
write.csv(PHtable, file = 'PHci.csv')
write.csv(KMbootstrap, file = 'KMbootstrap.csv')
write.csv(PHbootstrap, file = 'PHbootstrap.csv')
