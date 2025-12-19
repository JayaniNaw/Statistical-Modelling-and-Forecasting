#--------Salary Data Analysis-----------------#

#Starting with a new working space
rm(list=ls())

#Install libraries
library(glmnet)
library(gamlss)
library(gamlss.dist)
library(gamlss.ggplots)
library(ggplot2)

# Import the data file
mydata <- read.csv(file = "Salary_Data.csv", header = TRUE)

# Get a sample of 1000 rows from 6704 entries
set.seed(456)
index <- sample(nrow(mydata), 1000)
mydata_sample <- mydata[index, ]

# Check dimensions
dim(mydata_sample)

#Variable names
names(mydata_sample)
#Dimensions
dim(mydata_sample)
#First five records
head(mydata_sample)

# the structure of the data
str(mydata_sample)

# checking missing values
colSums(is.na(mydata_sample)) 

#Omit missing values
dtr <- na.omit(mydata_sample)
colSums(is.na(dtr))

# Replace labels with single label
dtr$Education.Level <- as.character(dtr$Education.Level)

# Replace variants of "Bachelors" with a single label
dtr$Education.Level[dtr$Education.Level
              %in% c("Bachelor's",
                     "Bachelor's Degree")] <- "Bachelors"

dtr$Education.Level[dtr$Education.Level
                    %in% c("Master's",
                           "Master's Degree")] <- "Master's"

dtr$Education.Level <- factor(dtr$Education.Level)
dtr$Education.Level

# checking missing values
colSums(is.na(dtr))

# plotting the data
plot(dtr)

salary <- dtr$Salary
histDist(salary)

#Check the correlation of variables
library(gamlss.foreach)
which.Data.Corr(dtr, r=0.7)
library(corrplot)
cc <- cor(dtr[sapply(dtr, is.numeric)])

corrplot(cc)

# look of outliers
plot(Salary~Age, data=dtr)
plot(Salary~factor(Education.Level), data=dtr)

#Model Selection
mNO<-gamlss(Salary~Age+
              factor(Education.Level)+
              Years.of.Experience,
            sigma.fo = ~Age+
              factor(Education.Level)+
              Years.of.Experience,
            data=dtr,
            family=NO)
#Residual plots
plot(mNO)
wp(mNO, ylim.all=3)

mGG<-gamlss(Salary~Age+
              factor(Education.Level)+
              Years.of.Experience,
            sigma.fo = ~Age+
              factor(Education.Level)+
              Years.of.Experience,
            nu.fo = ~factor(Education.Level),
            data=dtr,
            family=GG)

plot(mGG)
wp(mGG)

GAIC(mNO,mGG)

#choose distributions
D1 <- chooseDist(mNO, type="realplus", parallel="snow", ncpuss=9)
print(D1)

mBCCG<-gamlss(Salary~Age+
                factor(Education.Level)+
                Years.of.Experience,
              sigma.fo = ~Age+
                factor(Education.Level)+
                Years.of.Experience,
              nu.fo = ~factor(Education.Level),
              data=dtr,
              family=BCCG, trace = TRUE)

mBCCGo<-gamlss(Salary~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               sigma.fo = ~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               nu.fo = ~factor(Education.Level),
               data=dtr,
               family=BCCGo)

mBCT<-gamlss(Salary~Age+
               factor(Education.Level)+
               Years.of.Experience,
             sigma.fo = ~Age+
               factor(Education.Level)+
               Years.of.Experience,
             nu.fo = ~factor(Education.Level),
             tau.fo = ~Years.of.Experience+factor(Education.Level),
             data=dtr,
             family=BCT)

mBCTo<-gamlss(Salary~Age+
                factor(Education.Level)+
                Years.of.Experience,
              sigma.fo = ~Age+
                factor(Education.Level)+
                Years.of.Experience,
              nu.fo = ~factor(Education.Level),
              tau.fo = ~Years.of.Experience+factor(Education.Level),
              data=dtr,
              family=BCTo)


mBCPE<-gamlss(Salary~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               sigma.fo = ~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               nu.fo = ~factor(Education.Level),
               tau.fo = ~Years.of.Experience+factor(Education.Level),
               data=dtr,
               family=BCPE)

mBCPEo<-gamlss(Salary~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               sigma.fo = ~Age+
                 factor(Education.Level)+
                 Years.of.Experience,
               nu.fo = ~factor(Education.Level),
               tau.fo = ~Years.of.Experience+factor(Education.Level),
               data=dtr,
               family=BCPEo)

mGB2<-gamlss(Salary~Age+
               factor(Education.Level)+
               Years.of.Experience,
             sigma.fo = ~Age+
               factor(Education.Level)+
               Years.of.Experience,
             nu.fo = ~factor(Education.Level),
             tau.fo = ~Years.of.Experience+factor(Education.Level),
             data=dtr,
             family=GB2)


GAIC(mNO,mGG,mBCCG,mBCCGo,mBCT,mBCTo,mBCPE,mGB2)

plot(mBCT)
resid_qqplot(mBCT)
wp(mBCT)
model_wp(mNO,mBCT)
summary(mBCT)

#use smoothing functions
m1 <- gamlss(Salary~pb(Age)+
               factor(Education.Level)+
               pb(Years.of.Experience),
             sigma.fo = ~pb(Age)+
               factor(Education.Level)+
               pb(Years.of.Experience),
             nu.fo = ~factor(Education.Level),
             tau.fo = ~Years.of.Experience+factor(Education.Level),
             data=dtr,
             family=BCT)

m2 <- gamlss(Salary~cs(Age)*
               factor(Education.Level)+
               cs(Years.of.Experience),
             sigma.fo = ~cs(Age)+
               factor(Education.Level)+
               cs(Years.of.Experience),
             nu.fo = ~factor(Education.Level),
             tau.fo = ~cs(Years.of.Experience)+factor(Education.Level),
             data=dtr,
             family=BCT)

m3<- gamlss(Salary~pvc(Age)+
              factor(Education.Level)+
              pvc(Years.of.Experience),
            sigma.fo = ~pb(Age)+
              factor(Education.Level)+
              pvc(Years.of.Experience),
            nu.fo = ~factor(Education.Level),
            tau.fo = ~pvc(Years.of.Experience)+factor(Education.Level),
            data=dtr,
            family=BCT)

GAIC(mNO,mBCT,m1,m2)

plot(mBCT)
wp(mBCT)
dtr
newdt<-data.frame(Age=c(53),
                  Gender=c("Male"),
                  Education.Level=c("Master's"),
                  Years.of.Experience=c(25))

pred_BCT<-predict(mBCT, newdata=newdt, type="response")
pred_BCT

pred_m2<-predict(m2, newdata=newdt, type="response")
pred_m2

####=================================================================#############
#First Parametric model
mbct0 <- gamlss(Salary~1,family=BCT, data=dtr)
mbct1 <- stepGAICAll.B(mbct0, scope=list(lower=~1, 
                                         upper=~pb(Age)+
                                           factor(Gender)+
                                           factor(Education.Level)+
                                           pb(Years.of.Experience)),
                       parallel="snow", ncpuss=8)

# checking the model
mbct1
resid_plots(mbct1)
resid_plots(mbct1)
resid_wp(mbct1)
summary(mbct1)
