This project include three parts as Question1, Question2, Question3.
--
--Question 1--
BMI Distribution Modelling Using GAMLSS (Age 16–17)
Overview
This project analyses the distribution of Body Mass Index (BMI) for adolescents aged 16–17 years using the dbbmi dataset. The analysis applies Generalised Additive Models for Location, Scale and Shape (GAMLSS) to identify an appropriate parametric distribution that accurately captures the characteristics of BMI data.

Dataset

Source: dbbmi dataset

Target variable: Body Mass Index (BMI)

Subset: Individuals aged between 16 and 17 years

Methodology
1. Data Preparation

Cleared the R working environment and loaded required libraries

Subset the dataset to include only observations with ages between 16 and 17

Extracted BMI values for further analysis

2. Exploratory Data Analysis

Visualised the BMI distribution using a histogram with 30 bins

Added a kernel density curve to assess skewness and distribution shape

Confirmed that BMI is a continuous, strictly positive variable, guiding the choice of distributions

3. Distribution Modelling

Several parametric distributions suitable for positive continuous data were fitted using GAMLSS:

Exponential (EXP)

Gamma (GA)

Box–Cox Cole and Green (BCCG)

Box–Cox Power Exponential (BCPE)

Box–Cox t (BCT)

Age was incorporated as a covariate for location and shape parameters where appropriate to capture age-related variation.

4. Model Selection

Models were compared using the Generalised Akaike Information Criterion (GAIC)

The Box–Cox t (BCT) distribution was selected as the best-fitting model based on goodness of fit and model complexity

5. Model Diagnostics

Evaluated model adequacy using:

Residual Q–Q plots

Worm plots

Diagnostic plots of fitted values

Diagnostics indicated that the selected BCT model adequately represents the BMI distribution for this age group

6. Parameter Estimation

Extracted fitted values for the BCT model parameters:

μ (location) – central tendency

σ (scale) – variability

ν (skewness) – asymmetry

τ (kurtosis) – tail behaviour

Key Findings

BMI data for adolescents aged 16–17 exhibit skewness and heavy tails that are not well captured by simple normal models

The BCT distribution provides a flexible and robust fit by modelling location, scale, skewness, and kurtosis simultaneously

GAMLSS offers significant advantages for health-related data where distributional assumptions vary with age

Tools & Technologies

Programming Language: R

Libraries: gamlss, MASS

Techniques: Distribution fitting, GAIC-based model selection, residual diagnostics
