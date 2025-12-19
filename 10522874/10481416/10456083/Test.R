# Clear the current workspace
rm(list = ls())

# Load required packages
install.packages("gamlss")
install.packages("gamlss.data")
install.packages("gamlss.add")
install.packages("MASS")
library(gamlss)
library(gamlss.data)
library(gamlss.add)
library(MASS)

# Load the BMI dataset from gamlss.data
data(dbbmi)

# Set the starting age (modify this as needed for your assignment)
old <- 10

# Subset the data for boys aged strictly between 10 and 11
mydataset <- with(dbbmi, subset(dbbmi, age > old & age < old + 1))

# Extract only the BMI column for analysis
bmi10 <- mydataset$bmi

# Plot histogram using truehist with density line
truehist(bmi10, nbins = 30, col = "lightblue", xlab = "BMI", main = "Histogram of BMI (Age 10–11)", prob = TRUE)
lines(density(bmi10), col = "darkblue", lwd = 2)

# Fit various candidate distributions to the BMI data
fitted_models <- fitDist(bmi10, type = "realline")  # For distributions on the real line

# Display the list of fitted distributions
fitted_models$fits

# Normal distribution
m0 <- gamlss(bmi10 ~ 1, data = mydataset, family = NO)
wp(m0)  # Worm plot for checking residuals

# Box-Cox Power Exponential (BCPE)
m1 <- gamlss(
  bmi10 ~ 1,
  sigma.fo = ~ pbm(age, df = 4),
  nu.fo    = ~ pbm(age, df = 4),
  tau.fo   = ~ pbm(age, df = 3),
  data = mydataset,
  family = BCPE
)
wp(m1)  # Check worm plot

# Box-Cox t distribution (BCT)
m2 <- gamlss(
  bmi10 ~ 1,
  sigma.fo = ~ cs(age, df = 4),
  nu.fo    = ~ cs(age, df = 4),
  tau.fo   = ~ cs(age, df = 3),
  data = mydataset,
  family = BCT
)
wp(m2)

# Compare models using AIC, BIC, and GAIC
AIC(m0, m1, m2)
BIC(m0, m1, m2)
GAIC(m0, m1, m2, k = 3)  # GAIC with default penalty


# Print summary of the best model
summary(m1)
