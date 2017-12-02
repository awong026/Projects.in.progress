

##############################           Import Dataset (Sona Data and Sona_rating Data)              ##############################

#Sona
library(readr)
Sona <- read_csv("C:/Users/awong/Desktop/Memphis/Project/Sona.csv")
View(Sona)


#Sona_rating
library(readr)
Sona_rating <- read_csv("C:/Users/awong/Desktop/Memphis/Project/Sona_rating.csv")
View(Sona_rating)



#############################                               EDA                                      ###############################

######## Structure
str(Sona)
str(Sona_rating)


######## Summary
summary(Sona)
Summary(Sona_rating)


######## Correlation Plots
#Check correlation (Sona)
library(corrplot)
cor(Sona[,C(2:length(Sona))])
corrplot(cor(Sona))


#############################                               Clean Data                                          ###############################

#### Sona

#Let's calculate reading speed
Sona$Rspeed <- Sona$WordCount/Sona$RawRT
str(Sona)

#Take out abnormal data
Sona.nab <- Sona[which(MT$Abnormal_95 == 0),]
str(Sona.nab)

#### Sona_Rating

SR <- Sona_rating[,2:length(Sona_rating)]
View(SR)

str(SR)
SR$Difficulty <- as.factor(SR$Difficulty)
SR$Familarity <- as.factor(SR$Familarity)
SR$Interest <- as.factor(SR$Interest)
SR$Value <- as.factor(SR$Value)
SR$Boredom <- as.factor(SR$Boredom)
SR$MindWandering <- as.factor(SR$MindWandering)

#Combining Data set

library(dplyr)
data.j<-inner_join(x = Sona_rating, y = Sona, by = c("SSN", "GenreID"))
View(data.j)

#Split by genre to combine/cbind back together later
data.1 <- data.j[which(data.j$GenreID == 1),]
data.2 <- data.j[which(data.j$GenreID == 2),]
data.3 <- data.j[which(data.j$GenreID == 3),]
head(data.1)

#Now get each reader's data
data.1.ag<- aggregate(data.1[,c(2:6, 10, 13:length(data.1))], list(data.1$SSN), mean)
data.2.ag<- aggregate(data.2[,c(2:6, 10, 13:length(data.2))], list(data.2$SSN), mean)
data.3.ag<- aggregate(data.3[,c(2:6, 10, 13:length(data.3))], list(data.3$SSN), mean)
View(data.1.ag)

#Combine Rows into one dataframe
data.combo <- rbind(data.1.ag, data.2.ag, data.3.ag)
View(data.combo)

#Put in Rspeed
data.1.speedc<- aggregate(data.1[,c(11:12)], list(data.1$SSN), sum)
data.2.speedc<- aggregate(data.2[,c(11:12)], list(data.2$SSN), sum)
data.3.speedc<- aggregate(data.3[,c(11:12)], list(data.3$SSN), sum)

#Calc Speed
data.combo$Comp <- rbind(data.1.speedc, data.2.speedc, data.3.speedc)
head(data.combo)

data.combo$Rspeed <- data.combo$Comp$WordCount/data.combo$Comp$RawRT
head(data.combo)
#############################                           Hypothesis Testing                                      ###############################
##Question 2: Different Rspeed between the Genres
# 1 to 2
#1 to 3
#2 to 3

################################################################################


#Need to remember to load Rspeed code
One <- Sona[which(Sona$GenreID == 1),]
Two <- Sona[which(Sona$GenreID == 2),]
Three <- Sona[which(Sona$GenreID == 3),]
head(One)

#############  Genre 1 to Genre 2 comparison

par(mfrow = c(1,2))
boxplot(One$Rspeed) #There are some crazy outliers
boxplot(Two$Rspeed) #There are some crazy outliers


#check qqplot on diff on dist
par(mfrow=c(1,1))
qqplot(One$Rspeed,Two$Rspeed)  ##dist look diff (not 45 degree line) and also some crazy outliers

#Check for normality
qqnorm(One$Rspeed) ## Can't tell if normal since there are some crazy outliers
qqnorm(Two$Rspeed)  ## Cant'et tell if normal since there are some crazy outliers

##shaprio-wicks test: Okay for checking normal only. Could be better. Let's a lot in
shapiro.test(One$Rspeed) #Pvalue is below .001, so reject normal assumption
shapiro.test(Two$Rspeed) #Pvalue is below .001, so reject normal assumption 

#Check means
mean(One$Rspeed) #.0532 
mean(Two$Rspeed) #.00983 Means look to be really different from each other

#Check median since there are some crazy outliers
median(One$Rspeed) #.00317
median(Two$Rspeed) #.00338

#Check variance. If Var diff use welch if not you can use t test
var(One$Rspeed)
var(Two$Rspeed)

##Test to check for equal variances
var.test(One$Rspeed,Two$Rspeed, ratio = 1, alternative = "two.sided") # pvalue low and 1 is not in CI so very confident that the vars are not the same

#So we need to use welch's t test
#non par test for equal var
mood.test(One$Rspeed,Two$Rspeed, alternative = "two.sided") #Used non par test and the results agree. pvalue low

#Nonpar means
wilcox.test(One$Rspeed, Two$Rspeed, alternative = "two.sided", exact = F, method = median)  ##ties, pvalue is .005, Rspeed for Genre 1 and Genre 2 are nonidential populations

#Nonpar medians
str(One$Rspeed)
str(Two$Rspeed)
y <- c(One$Rspeed, Two$Rspeed)
str(y)
o = rep(1,times = 1826)
t = rep(2, times = 1858)
c = c(o,t)
str(c)


library(quantreg)
summary(rq(y~as.factor(c))) #pvalue below .0005 so differences in medians. 


#Let's try this using bootstrap method
library(boot)
med.diff <- function(d, i) {
  tmp <- d[i,] 
  median(tmp$Rspeed[tmp$GenreID=="1"]) - 
    median(tmp$Rspeed[tmp$GenreID=="2"])
}


boot.out <- boot(data = Sona, statistic = med.diff, R = 1000)

median(boot.out$t)

boot.ci(boot.out, type = "perc") #Since H0 is 0 because difference of means should be 0 if means are the same, we can reject H0 like before



#############  Genre 1 to Genre 3 comparison

par(mfrow = c(1,2))
boxplot(One$Rspeed) #There are some crazy outliers
boxplot(Three$Rspeed) #There are some crazy outliers


#check qqplot on diff on dist
par(mfrow=c(1,1))
qqplot(One$Rspeed,Three$Rspeed)  ##dist look diff (not 45 degree line) and also some crazy outliers

#Check for normality
qqnorm(One$Rspeed) ## Can't tell if normal since there are some crazy outliers
qqnorm(Three$Rspeed)  ## Cant'et tell if normal since there are some crazy outliers

##shaprio-wicks test: Okay for checking normal only. Could be better. Let's a lot in
shapiro.test(One$Rspeed) #Pvalue is below .001, so reject normal assumption
shapiro.test(Three$Rspeed) #Pvalue is below .001, so reject normal assumption 

#Check means
mean(One$Rspeed) #.0532 
mean(Three$Rspeed) #.032 Means look to be maybe the same

#Check median since there are some crazy outliers
median(One$Rspeed) #.00317
median(Three$Rspeed) #.00309

#Check variance. If Var diff use welch if not you can use t test
var(One$Rspeed)
var(Three$Rspeed)

##Test to check for equal variances
var.test(One$Rspeed,Three$Rspeed, ratio = 1, alternative = "two.sided") # pvalue low and 1 is not in CI so very confident that the vars are not the same

#So we need to use welch's t test
#non par test for equal var
mood.test(One$Rspeed,Three$Rspeed, alternative = "two.sided") #Used non par test and the results agree. pvalue low

#Nonpar means
wilcox.test(One$Rspeed, Three$Rspeed, alternative = "two.sided", exact = F, method = median)  ##ties, pvalue is .1493, Rspeed for Genre 1 and Genre 3 are on idential populations

#Nonpar medians
str(One$Rspeed)
str(Three$Rspeed)
y <- c(One$Rspeed, Three$Rspeed)
str(y)
o = rep(1,times = 1826)
t = rep(3, times = 1823)
c = c(o,t)
str(c)


library(quantreg)
summary(rq(y~as.factor(c))) #pvalue .19 so no differences in medians. 


#Let's try this using bootstrap method
library(boot)
med.diff <- function(d, i) {
  tmp <- d[i,] 
  median(tmp$Rspeed[tmp$GenreID=="1"]) - 
    median(tmp$Rspeed[tmp$GenreID=="3"])
}


boot.out <- boot(data = Sona, statistic = med.diff, R = 1000)

median(boot.out$t)

boot.ci(boot.out, type = "perc") #Since H0 is 0 because difference of means should be 0 if means are the same, we can't reject H0 like before. 0 is in 95% CI range. 

#############  Genre 2 to Genre 3 comparison

par(mfrow = c(1,2))
boxplot(Two$Rspeed) #There are some crazy outliers
boxplot(Three$Rspeed) #There are some crazy outliers


#check qqplot on diff on dist
par(mfrow=c(1,1))
qqplot(Two$Rspeed,Three$Rspeed)  ##dist look diff (not 45 degree line) and also some crazy outliers

#Check for normality
qqnorm(Two$Rspeed) ## Can't tell if normal since there are some crazy outliers
qqnorm(Three$Rspeed)  ## Cant'et tell if normal since there are some crazy outliers

##shaprio-wicks test: Okay for checking normal only. Could be better. Let's a lot in
shapiro.test(Two$Rspeed) #Pvalue is below .001, so reject normal assumption
shapiro.test(Three$Rspeed) #Pvalue is below .001, so reject normal assumption 

#Check means
mean(Two$Rspeed) #.0098
mean(Three$Rspeed) #.032 Means look to be maybe the same

#Check median since there are some crazy outliers
median(Two$Rspeed) #.00338
median(Three$Rspeed) #.00309

#Check variance. If Var diff use welch if not you can use t test
var(Two$Rspeed)
var(Three$Rspeed)

##Test to check for equal variances
var.test(Two$Rspeed,Three$Rspeed, ratio = 1, alternative = "two.sided") # pvalue low and 1 is not in CI so very confident that the vars are not the same

#So we need to use welch's t test
#non par test for equal var
mood.test(Two$Rspeed,Three$Rspeed, alternative = "two.sided") #Used non par test and the results agree. pvalue low

#Nonpar means
wilcox.test(Two$Rspeed, Three$Rspeed, alternative = "two.sided", exact = F, method = median)  ##ties, pvalue is below .005, Rspeed for Genre 2 and Genre 3 are  nonidential populations

#Nonpar medians
str(Two$Rspeed)
str(Three$Rspeed)
y <- c(Two$Rspeed, Three$Rspeed)
str(y)
o = rep(2,times = 1858)
t = rep(3, times = 1823)
c = c(o,t)
str(c)


library(quantreg)
summary(rq(y~as.factor(c))) #pvalue 0 so differences in medians. 


#Let's try this using bootstrap method
library(boot)
med.diff <- function(d, i) {
  tmp <- d[i,] 
  median(tmp$Rspeed[tmp$GenreID=="2"]) - 
    median(tmp$Rspeed[tmp$GenreID=="3"])
}


boot.out <- boot(data = Sona, statistic = med.diff, R = 1000)

median(boot.out$t)

boot.ci(boot.out, type = "perc") #Since H0 is 0 because difference of means should be 0 if means are the same, we reject H0 like before. 0 isn't in 95% CI range. 








#############################                       MAVOVA/ANOVA/Kruskia Wallis                                ###############################
   

####### Kruskal Wallis (One Way) 
#We want to see if mean Rspeed level is the same for each Difficulty Rating Group.

## Two assumptions to check 1. normal and 2. equal variances
## plot to see if the means look different
## and look for outliers - Has a devestating effect on ANOVA so if if there are outliers, you don't want to use F test
#plot
boxplot(data.combo$Rspeed ~ data.combo$Difficulty) #Doesn't look at means are different, but it does look like there might be outliers

##Look for normality
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 1)]) ##Normal plot for Rspeed if Difficulty = 1
#Doesn't look like a line, but looks like a curve. ANOVA really only worries about if tails look weird
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 2)]) #Doesn't look like a line, but looks like a curve.
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 3)]) #Doesn't look like a line, but looks like a curve.
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 4)]) #Doesn't look like a line, but looks like a curve.Top could be an outlier
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 5)]) #Doesn't look like a line, but looks like a curve.. Top looks like there is an outlier
qqnorm(data.combo$Rspeed[which(data.combo$Difficulty == 6)]) #Normal looking except there is an outlier at the top
#Not sure if data is normally distributed, F test probably isn't a good test to run for this comparison. 

##Look at variances. (Look to see if varaince is 10x bigger thna another. If they are then you don't use anova, but none of examples are that way)
var(data.combo$Rspeed[which(data.combo$Difficulty == 1)]) 
var(data.combo$Rspeed[which(data.combo$Difficulty == 2)]) 
var(data.combo$Rspeed[which(data.combo$Difficulty == 3)]) 
var(data.combo$Rspeed[which(data.combo$Difficulty == 4)]) 
var(data.combo$Rspeed[which(data.combo$Difficulty == 5)]) 
var(data.combo$Rspeed[which(data.combo$Difficulty == 6)]) 
#So we can assume constant variances


##fit the model (Creating anova) ##factor function really matters
fit <- aov(data.combo$Rspeed ~ as.factor(data.combo$Difficulty))
summary(fit) #Means are the same from looking at just the P value for F test since it's .3
#Don't reject H0: mean1 = mean2 = mean3 = mean 4 = mean 5 = mean 6


##Since normality was questionable and we saw outliers we can use a non par test which is the  (anova is really bad when there are outliers)
#Kruskal-Wallis test which is based on ranks
kruskal.test(data.combo$Rspeed ~ as.factor(data.combo$Difficulty))
#Also don't reject H0 because pvalue = .37

#To confirm findings even more let's try to do a mutliple comparison of the data and see if any pairs of data have different medians
TukeyHSD(fit)
plot(TukeyHSD(fit))





#############################                              Logistic Regression                                ###############################


####### Response is MindWandering
SR.MW <- SR[!is.na(SR$MindWandering),]
View(SR.MW)
fit <- glm(MindWandering ~ GenreID + Difficulty + Familarity + Interest + Value, family = binomial, data = SR.MW)
summary(fit)  #Sig are Difficulty (3, 4, 5), Interest (5,6)


#Let's throw fit into StepAIC to find the "best"
library(car)
library(MASS)
stepAIC(fit)

#Try fitting "best" model
AIC.fit <- glm(MindWandering ~ Interest + Difficulty, family = binomial, data = SR.MW)
summary(AIC.fit)
#AIC 373.09
#Residual deviance: 351.09  on 334  degrees of freedom
#Test pvalue
1-pchisq(351.09,334) #pvalue is .249 so H0. Model is adequate for the data. 

#Confusion Matrix to find Accuracy 
#Create coonfustion matrix to assess fit of model
predict <- predict(AIC.fit, type = "response")
table(SR.MW$MindWandering, predict>0.5) # Creates a confusion matrix which assess the fit of a model

###lOok into fitureing out % accuracy
(57 + 125 + 68 +9)/(57 + 125 + 68 +9 + 62+ 21 + 3) #Correct 75% of the time



#############################                                 Residuals                                      ###############################

####### Logistic Regression (Mindwandering)
plot(AIC.fit$residuals) #Looks like no pattern, but crazy outliers


################################                     Response is Boredom
SR.Boredom <- SR[!is.na(SR$Boredom),]
View(SR.Boredom)
fit <- glm(Boredom ~ GenreID + Difficulty + Familarity + Interest + Value, family = binomial, data = SR.Boredom)
summary(fit)  #Sig are Difficulty2, Interest all except Interst at 5


#Let's throw fit into StepAIC to find the "best"
library(car)
library(MASS)
stepAIC(fit)

#Try fitting "best" model
AIC.fit <- glm(Boredom ~ Interest, family = binomial, data = SR.Boredom)
summary(AIC.fit)
#AIC 171.6
#Residual deviance: 159.61  on 366  degrees of freedom
#Test pvalue
1-pchisq(159.61,366) #pvalue is 1 so H0. Model is adequate for the data. 

#Confusion Matrix to get Accuracy 
#Confusion Matrix to find Accuracy 
#Create coonfustion matrix to assess fit of model
predict <- predict(AIC.fit, type = "response")
table(SR.Boredom$Boredom, predict>0.5) # Creates a confusion matrix which assess the fit of a model

###lOok into fitureing out % accuracy
(16 + 42 + 69 + 84 + 65 +45)/(16 + 42 + 69 + 84 + 65 +45 + 36 + 5+4+2+1+3) #Correct 86% of the time
#############################                                 Residuals                                      ###############################

####### Logistic Regression (Boredom)
plot(AIC.fit$residuals) #Looks like no pattern, but crazy outliers



#############################                              Linear Regression                                 ###############################

####### Response as Rspeed with abnormals (Only Sona Data)
pairs(Sona[c("Rspeed", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")]) #Looks like a lot of outliers

model<- lm(Rspeed ~ Narrativity + SyntacticSimplicity + WordConcreteness + ReferentialCohesion + DeepCohesion , data = Sona) 
summary(model) #Only Narrativity sig
#adj. Rsquared is .001
##Ftest pvalue is .03 ,so model is good for data

#StepAIC to find "best" model
stepAIC(model)

#StepAIC's best model
model.AIC <- lm(Rspeed ~ Narrativity, data = Sona)
summary(model.AIC)
#AIC = -7453
#adj R = .001
#Ftest pvalue is .004, so model is good for data

#############################                                 Residuals                                      ###############################

####### Linear Regression
fitted <- predict(model.AIC)
resid <- residuals(model.AIC)

qqnorm(resid)  ##Can't tell if normal, crazy outliers
plot(resid)  ## Can't tell if pattern, doesn't look like it. But there are some crazy outliers

#Non - constant variances
plot(fitted, resid) # Doesn't look like there is a pattern, but with outliers. 

###### Response as Rspeed without abnormals  (Only Sona Data)
pairs(Sona.nab[c("Rspeed", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")]) #Looks like a lot of outliers

model<- lm(Rspeed ~ Narrativity + SyntacticSimplicity + WordConcreteness + ReferentialCohesion + DeepCohesion , data = Sona.nab) 
summary(model) #None are sig
#adj. Rsquared is 7.7 e-05
##Ftest pvalue is .37 ,so model is bad fit for data

#StepAIC to find "best" model
stepAIC(model)

#StepAIC's best model
model.AIC <- lm(Rspeed ~ SyntacticSimplicity + DeepCohesion , data = Sona.nab)
summary(model.AIC)
#AIC = -6581.53
#adj R = .0002
#Ftest pvalue is .21, so model is not good for data still if alpah is set to .05

#############################                                 Residuals                                      ###############################

####### Linear Regression
fitted <- predict(model.AIC)
resid <- residuals(model.AIC)

qqnorm(resid)  ##Can't tell if normal, crazy outliers
plot(resid)  ## Can't tell if pattern, doesn't look like it. But there are some crazy outliers

#Non - constant variances
plot(fitted, resid) # Doesn't look like there is a pattern, but with outliers. 


#############################                                 Citation                                      ###############################
citation("readr")
citation("corrplot")
citation("dplyr")
citation("quantreg")
citation("boot")
citation("car")
citation("MASS")


