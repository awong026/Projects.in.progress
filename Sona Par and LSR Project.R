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
Bob$Value <- as.factor(Bob$Interest)
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


##probably need to to logistic regression 