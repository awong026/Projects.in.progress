#Import Data

library(readr)
MT <- read_csv("C:/Users/awong/Desktop/Memphis/Project/MT.csv")
View(MT)

#EDA (Data rows are chunks of participants' reading not the whole thing)
summary(MT)

#Take out first column since its useless because all data from this dataset is from MT
MT<- (MT[,2:length(MT)])
summary(MT)

str(MT)


pairs(MT[c("RawRT", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")])

#pairs without abnormals
MT.nab <- MT[which(MT$Abnormal_95 == 0),]
pairs(MT.nab[c("RawRT", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")])

#boxplot
boxplot(MT.nab$Narrativity)

fit <- lm(WordCount ~ Narrativity *SyntacticSimplicity * WordConcreteness * ReferentialCohesion * DeepCohesion , data = MT.nab)
summary(fit)

#Let's Only look at genre one relationships with RawRT
MT.nab.g1 <- MT.nab[which(MT.nab$GenreID == 1),]

#model
fitg1 <- lm(RawRT ~ Narrativity * SyntacticSimplicity * WordConcreteness * ReferentialCohesion * DeepCohesion , data = MT.nab.g1)
summary(fitg1)

pairs(MT.nab.g1[c("RawRT", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")])
str(MT.nab.g1)


#Let's look at MT_Rating instead since the other one is crazy (Surveys)
library(readr)
MT_rating <- read_csv("C:/Users/awong/Desktop/Memphis/Project/MT_rating.csv")
View(MT_rating)

Bob <- MT_rating[,1:6]
View(Bob)

str(Bob)
Bob$GenreID <- as.factor(Bob$GenreID)
str(Bob)

fitr <- manova(cbind(Difficulty, Familarity, Interest, Value) ~ GenreID, data = Bob)
summary(fitr) #Low pvalue so there is a difference in means

summary.aov(fitr)
#Familarity has a difference and so does Value

#See which pairs of Famility are different for genreID
pairwise.t.test(Bob$Familarity, Bob$GenreID)
#tells me genre 1 and genre 2 have different means and genre 2 and 3 have different means for Familiarity

#See which pairs of Value are different for genreID
pairwise.t.test(Bob$Value, Bob$GenreID)
#tells me genre 1 and genre 2 have different means and genre 2 and 3 have different means for Value

#Looks at comparisons
plot(Bob$GenreID, Bob$Familarity) #Looks like Genre 2 has a lower famility than the others
plot(Bob$GenreID, Bob$Value) #Looks like means are fine, but Genre 2 is more bottom heavy

#Model
gl <- glm(GenreID ~ Familarity + Value, family = binomial, data = Bob)
summary(gl)
#Residual deviance: 485.85  on 381  degrees of freedom
#pvalue
1-pchisq(485.85,381) #model is not adquete. 

#Model2
glm<- glm(GenreID ~ Familarity , family = binomial, data = Bob)
summary(glm)


###########################################################################3
#Look at chance of abnormal 

###########################################################################

str(MT)

#Look at reading time with abnormal - Just to check what the prelim analysis says
mod <- lm(Abnormal_95 ~ RawRT, data = MT)
summary(mod)


#Let's calculate reading speed
MT$Rspeed <- MT$WordCount/MT$RawRT
str(MT)

#Look at reading speed with abnormal to check perlim analysis says
mod2 <- lm(Abnormal_95 ~ Rspeed, data = MT)
summary(mod2) #Reading speed is significant for determining abnormal like the prelim said. 




#Let's redo the first analysis with reading speed instead of RawRT
pairs(MT[c("Rspeed", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")]) #Looks like a lot of outliers

model<- lm(Rspeed ~ Narrativity *SyntacticSimplicity * WordConcreteness * ReferentialCohesion * DeepCohesion , data = MT) 
#Noting is sig
summary(model)

#Look at residuals plot
plot(Rspeed, resid(model),pch=16, 
     xlab="Rspeed Error", 
     ylab="Residual", 
     main="Residual Plot")

abline(h=0, lwd=2, lty=2, 
       col="blue") #Some how the residuals are linear, so not good. 

#Cook's distance for outliers
cooksd<- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red", pos = 2)  # add labels


#Now let's look/deal with the influential rows
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(MT[influential, ])


#Find most extreme row/obs
library(car)
outlierTest(model)
#   rstudent unadjusted p-value Bonferonni p
#1225 56.270879         0.0000e+00   0.0000e+00
#4841 42.428383         0.0000e+00   0.0000e+00
#2108 19.234238         6.7683e-80   3.8241e-76
#4826 16.923412         1.0592e-62   5.9842e-59
#2124 13.350995         4.7419e-40   2.6792e-36
#578   6.398995         1.6907e-10   9.5523e-07
#2096  6.166971         7.4466e-10   4.2073e-06
#4814  6.005441         2.0278e-09   1.1457e-05
#2129  5.236338         1.6976e-07   9.5912e-04
#4821  5.091843         3.6617e-07   2.0688e-03

#Try taking out outliers and see what happens ##################################3
new.MT <- MT[-c(1225, 4841, 2108, 4826, 2124, 576, 2096, 4814, 2129, 4821),]

#Create model with reading speed again, but with outliers gone
pairs(new.MT[c("Rspeed", "Narrativity", "SyntacticSimplicity" , "WordConcreteness", "ReferentialCohesion", "DeepCohesion")]) #Looks like a lot of outliers

model.new<- lm(Rspeed ~ Narrativity +SyntacticSimplicity + WordConcreteness + ReferentialCohesion + DeepCohesion , data = new.MT) 
summary(model.new) #sig factors are SyntacticsSimplicity, WordConcetreness, ReferentialChoesion


#Let's use stepAIC to find "best" model. Nice cause deals with multicolinearity too, even though I forgot to check. 
library(MASS)
stepAIC(model.new)

#This was the "best model"

#Step:  AIC=-28485.51
#Rspeed ~ SyntacticSimplicity + WordConcreteness + ReferentialCohesion

#Df Sum of Sq    RSS    AIC
#<none>                             36.075 -28486
#- WordConcreteness     1  0.079712 36.155 -28475
#- ReferentialCohesion  1  0.080548 36.155 -28475
#- SyntacticSimplicity  1  0.106713 36.182 -28471

#Call:
 # lm(formula = Rspeed ~ SyntacticSimplicity + WordConcreteness + 
  #     ReferentialCohesion, data = new.MT)

#Coefficients:
 # (Intercept)  SyntacticSimplicity     WordConcreteness  ReferentialCohesion  
#0.021320             0.003905            -0.002649             0.003351  

best.model <- lm(Rspeed ~ SyntacticSimplicity + WordConcreteness + ReferentialCohesion, data = new.MT)
summary(best.model) #All sig
#F test pvalue is below .05
#adj R sqaured is .005

#Let's see interaction effect
best.model <- lm(Rspeed ~ SyntacticSimplicity * WordConcreteness * ReferentialCohesion, data = new.MT)
summary(best.model) #All factors sig and some interaction effects sig
stepAIC(best.model) #Final model's AIC worse

####################################         Note: Rsquared in every model is really small         ############################
#Redo but take out abnormals and outliers
MT.nab$Rspeed <- MT.nab$WordCount/MT.nab$RawRT


model.nab<- lm(Rspeed ~ Narrativity +SyntacticSimplicity + WordConcreteness + ReferentialCohesion + DeepCohesion , data = MT.nab) 
#Noting is sig
summary(model.nab)

#Cook's distance for outliers
cooksd<- cooks.distance(model.nab)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red", pos = 2)  # add labels


#Now let's look/deal with the influential rows
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(MT[influential, ])


#Find most extreme row/obs
library(car)
outlierTest(model.nab)

#Take out outliers
new.MT.nab <- MT.nab[-c(914, 3603, 1571, 3588, 1587, 3577, 1592, 1590, 3584),]

#refit the model
new.model.nab<- lm(Rspeed ~ Narrativity +SyntacticSimplicity + WordConcreteness + ReferentialCohesion + DeepCohesion , data = new.MT.nab) 
summary(new.model.nab)

outlierTest(new.model.nab)

#StepAIC
stepAIC(new.model.nab)

#best model from StepAIC
best.model.nab <- lm(Rspeed ~ SyntacticSimplicity + WordConcreteness + ReferentialCohesion, data = new.MT.nab)
summary(best.model.nab) #It's all sig, adj R squared is .9% , F test pvalue is below .05 so good. 


#Split data in genres
best.model.nabg <- lm(Rspeed ~ SyntacticSimplicity + WordConcreteness + ReferentialCohesion, data = new.MT.nab[which(new.MT.nab$GenreID == 1),])
summary(best.model.nabg) #It's all sig, adj R squared is 1% , F test pvalue is below .05 so good. 


#Check residuals but since R squared sooo low there doesn't seem to be a point #######################################################

################################3

#new study: Look for reading speed depending on factors from rating table

################################

###Maybe join after we avg the data for each reading. 

library(dplyr)
data.j<-inner_join(x = MT_rating, y = MT, by = c("SSN", "GenreID"))

#Split by genre to combine/cbind back together later
data.1 <- data.j[which(data.j$GenreID == 1),]
data.2 <- data.j[which(data.j$GenreID == 2),]
data.3 <- data.j[which(data.j$GenreID == 3),]
head(data.1)

#Now get each reader's data
data.1.ag<- aggregate(data.1[,c(2:6, 10:length(data.1))], list(data.1$SSN), mean)
data.2.ag<- aggregate(data.2[,c(2:6, 10:length(data.2))], list(data.2$SSN), mean)
data.3.ag<- aggregate(data.3[,c(2:6, 10:length(data.3))], list(data.3$SSN), mean)
View(data.1.ag)

#Combine Rows into one dataframe
data.combo <- rbind(data.1.ag, data.2.ag, data.3.ag)
View(data.combo)



##########################                   Analysis time                         #######################################

pairs(data.combo[c("Rspeed", "Difficulty", "Familarity", "Interest", "Value")]) #Looks like there are some outliers

fit <- lm(Rspeed ~ Difficulty + Familarity + Interest + Value, data = data.combo)
summary(fit) #Only Interest and Value are sig and adj Rsqaured is .028
#pvalue is good, it's below .05

#Let's try taking out the outliers 
library(car)
outlierTest(fit)


#Take out outliers
data.combo.nout <- data.combo[-c(366, 305, 238, 29, 110, 177, 49),]
View(data.combo.nout)

#Pairs
pairs(data.combo.nout[c("Rspeed", "Difficulty", "Familarity", "Interest", "Value")]) 

#Create model
fit <- lm(Rspeed ~ Difficulty + Familarity + Interest + Value, data = data.combo.nout)
summary(fit) #Interest and Value sig, but adj Rsquared is .005
outlierTest(fit)


#Take out outliers
data.combo.nout <- data.combo.nout[-c(288, 124, 16, 14, 128),]
View(data.combo.nout)

fit <- lm(Rspeed ~ Difficulty + Familarity + Interest + Value, data = data.combo.nout)
summary(fit) #Interest and Value sig, but adj Rsquared is .008
outlierTest(fit)


