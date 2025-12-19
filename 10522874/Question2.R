#Starting with a new working space
rm(list=ls())
dev.off()

library(gamlss)
library(gamlss.ggplots)
library(gamlss.dist)

#read the dataset
data(grip)
head(grip)

#select a sample using unique seed number
set.seed(2865) 
index<-sample(3766, 1000) 
mydata<-grip[index, ] 
dim(mydata) 

#Plotting the data
plot(grip ~ age, data = mydata, col="lightblue",
     xlab="Age",ylab="Grip")

# use LMS method to fit the data
mbccg <- gamlss(grip ~ pb(age),
                   sigma.fo = ~pb(age),
                   nu.fo = ~pb(age),
                   data = mydata,
                   family = BCCG)

#Check model degree of freedom
edf(mbccg) 

#Fitting BCT and BCPE models
mbct <- gamlss(grip ~ pb(age),
               sigma.fo = ~pb(age),
               nu.fo = ~pb(age),
               tau.fo = ~pb(age),
               data = mydata,
               family = BCT,
               start.from = mbccg)

mbcpe <- gamlss(grip ~ pb(age),
                sigma.fo = ~pb(age),
                nu.fo = ~pb(age),
                tau.fo = ~pb(age),
                data = mydata,
                family = BCPE,
                start.from = mbccg)

#Degrees of Freedom fitted for the Parameters
edfAll(mbct)

edfAll(mbcpe)

#Apply GAIC
GAIC(mbccg, mbct, mbcpe)

#Fitted parameters for the fitted models 
fittedPlot(mbccg, mbct, mbcpe, x=mydata$age) 

#Obtain a Centile Plot for the Fitted Models 
centiles(mbccg, xvar = mydata$age)

centiles(mbct, xvar = mydata$age)

centiles(mbcpe, xvar = mydata$age)

#Diagnostics
plot(mbccg)
resid_qqplot(mbccg)
wp(mbccg)
resid_wp(mbccg)

plot(mbct)
resid_qqplot(mbct)
wp(mbct)
resid_wp(mbct)


plot(mbcpe)
resid_qqplot(mbcpe)
wp(mbcpe)
resid_wp(mbcpe)


## Q.statistics
Q.stats(mbccg, xvar=mydata$age)
Q.stats(mbct, xvar=mydata$age)
Q.stats(mbcpe, xvar=mydata$age)

summary(mbct)
