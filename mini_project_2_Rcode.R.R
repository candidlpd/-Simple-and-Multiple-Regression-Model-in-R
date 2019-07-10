#a)Exploratory Data Analysis
#1)Understing the data
#load data

prostate <- read.csv("C:/Users/LPD/Desktop/Stat ML Pankaj/Project/project2/prostate_cancer.csv",head=TRUE, sep=",")
attach(prostate)
#know the dimensions of the data
dim(prostate)
#know the column names
colnames(prostate)
#know the data tpes of each variable
str(prostate)
head(prostate)

#checking NA(missing values) in datasets
anyNA(prostate)
colSums(sapply(prostate, is.na))
#2) data type convrsion like int into factor
vesinv<-factor(vesinv)
str(vesinv)
#3)Summary of each and every variable
summary(prostate)
#4)visualization
#histogram of all variables
library(purrr)
library(tidyr)
library(ggplot2)

prostate %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#density plot
prostate %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density() 

#boxplot 
boxplot(prostate)

#scatterplot matrix

require(corrplot)
pairs(prostate)
corvalue<-cor(prostate)
corrplot(corvalue, method="number")

#b) checking response varaible,  transformation needs or not
modfirst<-lm(psa~., data=prostate)
par(mfrow=c(2,2))
plot(modfirst)


e1<-resid(modfirst)
par(mfrow=c(1,1))
hist(e1)
shapiro.test(e1)



library(rcompanion)
plotNormalHistogram(psa, main="Checking normal histogram")
qqnorm(psa, main="Checking normality in QQplot")
qqline(psa, col="red")
shapiro.test(psa)
#checking after transformation
#for residuals
modsecond<-lm(log(psa)~., data=prostate)
par(mfrow=c(1,1))
plot(modsecond)
hist(resid(modsecond))
plotNormalHistogram(resid(modsecond), main="residual distribution after log transformation")
plotNormalHistogram(log(psa), main="response distribution after log transformation of psa")
#for response
plotNormalHistogram(sqrt(psa), main="square root transformation")
plotNormalHistogram(sign(psa)*abs(psa)^(1/3), main="cube root transformation")
plotNormalHistogram(log(psa), main="response distribution after log transformation of psa")
qqnorm(log(psa), main="Checking normality in QQplot after log transformation")
qqline(log(psa), col="red")
#normality test 
shapiro.test(log(psa))

#c)fitting a simple linear regression model for each predictor

logpsa<-log(psa)
mod_cancervol<-lm(logpsa~cancervol)
summary(mod_cancervol)
plot(logpsa~cancervol, pch=16, cex=1.3,col="blue", main="plot of logpsa vs cancervol")
abline(mod1, col="red", lwd=2)
# lm_coef <- round(coef(mod1), 3) # extract coefficients
# mtext(bquote(logpsa == .(lm_coef[2])*cancervol + .(lm_coef[1])),
#       adj=1, padj=0) # display equation

# segments(cancervol, fitted(mod1), cancervol,logpsa, col="green")

mod_weight<-lm(logpsa~weight)
summary(mod_weight)
plot(logpsa~weight, pch=16, cex=1.3,col="blue", main="plot of logpsa vs weight")
abline(mod_weight, col="red", lwd=2)


mod_age<-lm(logpsa~age)
summary(mod_age)
plot(logpsa~age, pch=16, cex=1.3,col="blue", main="plot of logpsa vs age")
abline(mod_age, col="red", lwd=2)


mod_benpros<-lm(logpsa~benpros)
summary(mod_benpros)
plot(logpsa~benpros, pch=16, cex=1.3,col="blue", main="plot of logpsa vs benpros")
abline(mod_benpros, col="red", lwd=2)

mod_vesinv<-lm(logpsa~vesinv)
summary(mod_vesinv)
plot(logpsa~vesinv, pch=16, cex=1.3,col="blue", main="plot of logpsa vs vesinv")
abline(mod_vesinv, col="red", lwd=2)

mod_capspen<-lm(logpsa~capspen)
summary(mod_capspen)
plot(logpsa~capspen, pch=16, cex=1.3,col="blue", main="plot of logpsa vs capspen")
abline(mod_capspen, col="red", lwd=2)

mod_gleason<-lm(logpsa~gleason)
summary(mod_gleason)
plot(logpsa~gleason, pch=16, cex=1.3,col="blue", main="plot of logpsa vs gleason")
abline(mod_gleason, col="red", lwd=2)
#d) fitting multiple regression model

model_full<-lm(logpsa ~ cancervol+weight+age+benpros+vesinv+capspen+gleason)
summary(model_full) 
sigma(model_full)/mean(logpsa)
#e)Build a good model
fit7<-lm(logpsa~cancervol+benpros+vesinv+gleason+capspen+age+weight)
fit4<-lm(logpsa~cancervol+benpros+vesinv+gleason)
fit3<-lm(logpsa~cancervol+benpros+vesinv)

fit3<-lm(logpsa~cancervol+benpros+gleason)

fit333<-lm(logpsa~cancervol+vesinv+gleason)
fit3333<-lm(logpsa~gleason+vesinv+benpros)
anova(fit7, fit4)
anova(fit4, fit3)
anova(fit4, fit33)
anova(fit4, fit333)
anova(fit4,fit3333)

#interaction plot 
ggplot(prostate)+
  aes(x=cancervol, y=logpsa, color=factor(vesinv))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(prostate)+
  aes(x=cancervol, y=logpsa, color=factor(benpros))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(prostate)+
  aes(x=cancervol, y=logpsa, color=factor(gleason))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(prostate)+
  aes(x=benpros, y=logpsa, color=factor(vesinv))+
  geom_point()+
  geom_smooth(method="lm")
ggplot(prostate)+
  aes(x=benpros, y=logpsa, color=factor(gleason))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(prostate)+
  aes(x=vesinv, y=logpsa, color=factor(gleason))+
  geom_point()+
  geom_smooth(method="lm")




#lm(y~x1*x2) is same as lm(y~x1+x2 +x1:x2)
#checking for intereaction by using all possible model

fitI1<-lm(logpsa~cancervol*benpros)
fitI2<-lm(logpsa~cancervol*vesinv)
fitI3<-lm(logpsa~cancervol*gleason)
fitI4<-lm(logpsa~benpros*vesinv)
fitI5<-lm(logpsa~benpros*gleason)
fitI6<-lm(logpsa~vesinv*gleason)
anova(fitI1)
anova(fitI2)
anova(fitI3)
anova(fitI4)
anova(fitI5)
anova(fitI6)

#multiple regression model using interaction
mod3<-lm(logpsa~cancervol+benpros+vesinv+gleason+cancervol:vesinv)
mod2<-lm(logpsa~cancervol+benpros+vesinv+gleason+benpros:vesinv)
mod1<-lm(logpsa~cancervol+benpros+vesinv+gleason)
anova(mod1,mod2)
anova(mod1,mod3)




#final model
mod1<-lm(logpsa~cancervol+benpros+vesinv+gleason)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
sigma(mod1)/mean(logpsa)



#checking for normality
shapiro.test(residuals(mod1))
par(mfrow=c(1,1))
# distribution of studentized residuals

library(MASS)
sresid <- studres(lm.fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals", xlab="studentized residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# non-constant error variance test
#Breush Pagan Test

lmtest::bptest(mod1)
car::ncvTest(mod1)



#testing the independence assumptions
library(car)
dwt(mod1)


#Testing independence

par(mfrow=c(1,1))
plot(residuals(mod1), xlab="state",ylab="residual",main="residual sequenceplot",type="l")
abline(h=0)
#g)
 table(vesinv)
# mean(cancervol)
# mean(benpros)
# mean(gleason)

vesinv <- factor(vesinv)


predict(mod1,data.frame(cancervol=mean(cancervol),benpros=mean(benpros),vesinv="0",gleason=mean(gleason)))
psa<-exp(2.330541)
psa

 

predict(mod1,data.frame(cancervol=mean(cancervol),benpros=mean(benpros),vesinv="0",gleason=mean(gleason)), se.fit=T, interval="prediction")

















