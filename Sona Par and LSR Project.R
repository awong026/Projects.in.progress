#Start at looking at how survey ratings effect boredom and mindwandering
#Multivariate multiple regression

library(readr)
Sona_rating <- read_csv("C:/Users/awong/Desktop/Memphis/Project/Sona_rating.csv")
View(Sona_rating)

#I want to determine when boredom or mindwandering happens. The responses are factors and so are the ind variables. Let's try doing a MANoVA

Bob <- Sona_rating[,2:length(Sona_rating)]
View(Bob)

str(Bob)
Bob$GenreID <- as.factor(Bob$GenreID)
Bob$Difficulty <- as.factor(Bob$Difficulty)
Bob$Familarity <- as.factor(Bob$Familarity)
Bob$Interest <- as.factor(Bob$Interest)
Bob$Value <- as.factor(Bob$Value)
Bob$Boredom <- as.factor(Bob$Boredom)
Bob$MindWandering <- as.factor(Bob$MindWandering)
str(Bob)

fitrg <- manova(cbind(Boredom, MindWandering) ~ GenreID, data = Bob)
summary(fitrg) #pvalue is above .05 so GenreID is no difference of means in boredom or mindwandering because of GenreID

fitrd <- manova(cbind(Boredom, MindWandering) ~ Difficulty, data = Bob)
summary(fitrd) #pvalue is low so there is a difference in means

fitrf <- manova(cbind(Boredom, MindWandering) ~ Familarity, data = Bob)
summary(fitrf) #pvalue is low so there is a difference in means

fitri <- manova(cbind(Boredom, MindWandering) ~ Interest, data = Bob)
summary(fitri) #pvalue is low so there is a difference in means

fitrv <- manova(cbind(Boredom, MindWandering) ~ Value, data = Bob)
summary(fitrv) #pvalue is low so there is a difference in means



summary.aov(fitrd)
#Boredom and Mindwandering has a difference

summary.aov(fitrf)
#Boredom and Mindwandering has a difference

summary.aov(fitri)
#Boredom and Mindwandering has a difference

summary.aov(fitrv)
#Boredom and Mindwandering has a difference


#See which pairs of Boredom are different for difficulty
TukeyHSD(fitrd)
#tells me genre 1 and genre 2 have different means and genre 2 and 3 have different means for Familiarity

#See which pairs of Value are different for genreID
pairwise.t.test(Bob$Value, Bob$GenreID)
#tells me genre 1 and genre 2 have different means and genre 2 and 3 have different means for Value

#Looks at comparisons
plot(Bob$GenreID, Bob$Familarity) #Looks like Genre 2 has a lower famility than the others
plot(Bob$GenreID, Bob$Value) #Looks like means are fine, but Genre 2 is more bottom heavy


##probably need to to logistic regression becasue that's what the project is about
#Need to take out NA for Boredom to study Boredom

Bob.Boredom <- Bob[!is.na(Bob$Boredom),]
View(Bob.Boredom)
fit <- glm(Boredom ~ GenreID + Difficulty + Familarity + Interest + Value, family = binomial, data = Bob.Boredom)
summary(fit)
#Sig are Difficulty2, Familarity2, Interest all except Interst at 5

#Let's throw fit into StepAIC to find the "best"
library(car)
library(MASS)
stepAIC(fit)

#Try fitting "best" model
AIC.fit <- glm(Boredom ~ Interest, family = binomial, data = Bob.Boredom)
summary(AIC.fit)
#AIC 171.6
#Residual deviance: 159.61  on 366  degrees of freedom
#Test pvalue
1-pchisq(159.61,366) #pvalue is 1 so H0

#Presudo Rsquared
library(rcompanion)
nag


#How to find Rsquared of glm (Or use R package), then you lrm fucntion to make logistic regression. Sicne different coefficents need to check for constant variances
library(rms)
fit <- lrm(Boredom ~ Interest, data = Bob.Boredom)
print(fit) #Rsquared is .584



######################### Let's try it with Mindwandering instead ###########################

#Take out Mindwandering Na rows
Bob.MW <- Bob[!is.na(Bob$MindWandering),]
fit <- glm(MindWandering ~ GenreID + Difficulty + Familarity + Interest + Value, family = binomial, data = Bob.MW)
summary(fit) #Some Difficutlies cause mindwandering and some Interest cause Mindwandering 

#StepAIC function
stepAIC(fit)

#AIC says that best model is:

AIC.fit <- glm(formula = MindWandering ~ Difficulty + Interest, family = binomial, 
               data = Bob.MW)
summary(AIC.fit)
#AIC = 373.09
#Residual deviance: 351.09  on 334  degrees of freedom
#pvalue 
1-pchisq(351.09,334) #pvalue = .24, which means H0: model is adequate for the data. 


#Try using lrm fucntion
fit <- lrm(MindWandering ~ Interest + Difficulty, data = Bob.MW)
print(fit)  #Rsquared is .33
