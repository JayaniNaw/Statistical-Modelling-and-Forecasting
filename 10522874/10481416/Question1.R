#Question 1

#Starting with a new working space
rm(list=ls())

#Bring required packages
install.packages("gamlss.add")
install.packages("ggplot2")

library(gamlss)
library(MASS)

data(dbbmi)

#--------------------------(a)--------------------------------------------------
#get age between sixteen to Seventeen
old <- 16

#create subset from the main data set that includes age range 16 to 17
mydataset <- with(dbbmi, subset(dbbmi, age > old & age < old + 1))

#bmi only contains the ages between 16 to 17
bmi17 <- mydataset$bmi 

#plot the histogram
truehist(bmi17, nbins = 30, col = "lightblue", xlab = "BMI", main = "Histogram of BMI (Age 16-17)", prob = TRUE)

# Add density line
lines(density(bmi17), col = "darkblue", lwd = 2)

#histDist(bmi17)


#-------------------------(b)---------------------------------------------------
#Fit different parametric distributions to the data and
#choose an appropriate distribution to the data

#BMI is continous distribution but cannot be a negative value.
#Therefore, it can be only taken Continuous distributions on the positive real line, R+ = (0, ∞)

#Normal Distribution
mno<-gamlss(bmi ~ age, sigma.fo = ~ age, data = mydataset, family = NO)
plot(mno)
resid_qqplot(mno)
wp(mno)
resid_wp(mno)

#exponential Distribution
mexp<-gamlss(bmi ~ age, data = mydataset, family = EXP)

#gamma Distribution
mga<-gamlss(bmi ~ age, sigma.fo = ~ age, data = mydataset, family = GA)

#Box-Cox Cole and Green Distribution
mbccg<-gamlss(bmi ~ age, sigma.fo = ~ age, nu.fo = ~ age, data = mydataset, family = BCCG)

#Box-Cox Power Exponential
mbcpe<-gamlss(bmi ~ age, sigma.fo = ~ age, nu.fo = ~ age, tar.fo = ~ age, data = mydataset, family = BCPE)

#Box-Cox t Distribution
mbct<-gamlss(bmi ~ age, sigma.fo = ~ age, nu.fo = ~ age, tar.fo = ~ age, data = mydataset, family = BCT)

#apply generalised Arkaike Information Criteria
GAIC(mexp,mga,mbccg,mbcpe,mbct)
#GAIC(m6,m7,k=3)

plot(mbct)
resid_qqplot(mbct)
wp(mbct)
resid_wp(mbct)
summary(mbct)

#fitted parameter values of the final chosen model

mydataset$age[1]
rownames(mydataset)[1]

fitted(mbct, "mu")[1]
fitted(mbct, "sigma")[1]
fitted(mbct, "nu")[1]
fitted(mbct, "tau")[1]




#df- degrees of freedom to avoid overfitting






