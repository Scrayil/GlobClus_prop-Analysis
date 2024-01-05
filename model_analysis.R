# Copyright (C) [2024] [Mattia Bennati]

# DATASET 1 GlobClus_prop
#### ASTRO DATA: 20 different measures for 147 globular star clusters in the Milky Way Galaxy
# https://astrostatistics.psu.edu/MSMA/datasets/index.html
# Properties include Galactic location, integrated stellar luminosity, metallicity, ellipticity,
# central surface brightness, color, and seven measures of dynamical state
# (core and tidal radius, concentration, central star density and relaxation time,
# central velocity dispersion and escape velocity).
# https://search.r-project.org/CRAN/refmans/astrodatR/html/GlobClus_prop.html
# data description: Appendix C.7 of Feigelson & Babu (2012)
# with statistical analysis in Chapter 8.

# Importing packages
#install.packages("igraph")
#install.packages("RcppEigen")
#install.packages("RcppArmadillo")
#install.packages("gRbase")
#install.packages("gRain")
#install.packages("gRim")
library(gRbase)
library(gRain)
library(gRim)
library(bnlearn)

# Clears the current environment
rm(list=ls(all=TRUE))
# Sets the current working directory
setwd("~/Desktop/university/AI/FOSM/project/GlobusClus_prop-Analysis/")

# Installing the "astrodatR" dataset
# install.packages("astrodatR")
# Importing the dataset
require(astrodatR)
# Parsing the dataset data
data(GlobClus_prop)
# Saves the data into the dat variable
dat <- GlobClus_prop

# Prints the first 5 rows of the dataset
# print(head(dat, n=5))


# CHOSEN OBJECTIVE:
# Study of the dynamics of globular clusters
# Identifying which parameters influence the dispersion of the
# central velocity of a globular star cluster and how they affect it


# PHASE 1:
# IDENTIFICATION OF VARIABLES
# Name: Common name
# Gal.long: Galactic longitude (degrees)
# Gal.lat: Galactic latitude (degrees)
# R.sol: Distance from the Sun (kiloparsecs, kpc)
# R.GC: Distance from the Galactic Center (kpc)
# Metal: Log metallicity with respect to solar metallicity
# Mv: Absolute magnitude
# r.core: Core radius (parsecs, pc)
# r.tidal: Tidal radius (pc)
# Conc: Core concentration parameter
# log.t: Log central relaxation timescale (years)
# log.rho: Log central density (solar masses per cubic parsec)
# S0: Central velocity dispersion (kilometers per second)
# V.esc: Central escape velocity (km/s)
# VHB: Level of the horizontal branch (magnitude)
# E.B-V: Color excess (magnitude)
# B-V: Color index (magnitude)
# Ellipt: Ellipticity
# V.t: Integrated V magnitude (magnitude)
# CSBt: Central surface brightness (magnitude per square arcsecond)

# FASE 2:
# PULIZIA DEL DATASET DAI VALORI NAN (CON DATI INCOMPLETI) E ESCLUSIONE DELLA
# COLONNA DEI NOMI IN QUANTO SUPERFLUA PER LO STUDIO
dat <- na.omit(dat[,-1]);
# Primi 5 record del dataset
head(dat, n=5)

# PHASE 3:
# DEFINITION OF VARIABLES
y = dat$S0
x1 = dat$Gal.long
x2 = dat$Gal.lat
x3 = dat$R.sol
x4 = dat$R.GC
x5 = dat$Metal
x6 = dat$Mv
x7 = dat$r.core
x8 = dat$r.tidal
x9 = dat$Conc
x10 = dat$log.t
x11 = dat$log.rho
x12 = dat$V.esc
x13 = dat$VHB
x14 = dat$E.B.V
x15 = dat$B.V
x16 = dat$Ellipt
x17 = dat$V.t
x18 = dat$CSBt

# PHASE 4:
# ANALYSIS OF SIGNIFICANT VARIABLES:
# Definition of the complete model:
mq <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18)
summary(mq)

# Definition of the null model (intercept only):
mq0 <- lm(y ~ 1)
summary(mq0)

# 1st - P-VALUE STRATEGY WITH "BACKWARD" METHOD:
# Starting from the complete model, iteratively eliminate the variable
# with the highest p-value (Pr(>|t|)) until there are no non-significant variables
mq1 <- update(mq, .~. -x9)
summary(mq1)

mq1 <- update(mq1, .~. -x8)
summary(mq1)

mq1 <- update(mq1, .~. -x16)
summary(mq1)

mq1 <- update(mq1, .~. -x4)
summary(mq1)

mq1 <- update(mq1, .~. -x17)
summary(mq1)

mq1 <- update(mq1, .~. -x11)
summary(mq1)

mq1 <- update(mq1, .~. -x15)
summary(mq1)

mq1 <- update(mq1, .~. -x2)
summary(mq1)

mq1 <- update(mq1, .~. -x3)
summary(mq1)

mq1 <- update(mq1, .~. -x13)
summary(mq1)

mq1 <- update(mq1, .~. -x1)
summary(mq1)

mq1 <- update(mq1, .~. -x7)
summary(mq1)

mq1 <- update(mq1, .~. -x5)
summary(mq1)

# IDENTIFIED MODEL:
# lm(formula = y ~ x6 + x10 + x12 + x14 + x18)
# Summary information of the model:
summary(mq1)

# 2nd - P-VALUE STRATEGY WITH "FORWARD" METHOD:
# starting from the null model (intercept only), iteratively add
# a variable with the lowest p-value
mq2 <- update(mq0, .~. +x1)
summary(mq2)

mq2 <- update(mq0, .~. +x2)
summary(mq2)

mq2 <- update(mq0, .~. +x3)
summary(mq2)

mq2 <- update(mq0, .~. +x4)
summary(mq2)

mq2 <- update(mq0, .~. +x5)
summary(mq2)

mq2 <- update(mq0, .~. +x6)
summary(mq2)

mq2 <- update(mq0, .~. +x7)
summary(mq2)

mq2 <- update(mq0, .~. +x8)
summary(mq2)

mq2 <- update(mq0, .~. +x9)
summary(mq2)

mq2 <- update(mq0, .~. +x10)
summary(mq2)

mq2 <- update(mq0, .~. +x11)
summary(mq2)

mq2 <- update(mq0, .~. +x12)
summary(mq2)

mq2 <- update(mq0, .~. +x13)
summary(mq2)

mq2 <- update(mq0, .~. +x14)
summary(mq2)

mq2 <- update(mq0, .~. +x15)
summary(mq2)

mq2 <- update(mq0, .~. +x16)
summary(mq2)

mq2 <- update(mq0, .~. +x17)
summary(mq2)

mq2 <- update(mq0, .~. +x18)
summary(mq2)

# Add x6 to the final model (minimum p-value)
mq2f <- update(mq0, .~. +x6)

# Continue iteratively:
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x9)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x12)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Add x12 (minimum p-value)
mq2f <- update(mq2f, .~. +x12)

# New iteration
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x9)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Add x9 (minimum p-value)
mq2f <- update(mq2f, .~. +x9)

# New iteration
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Add x11 (minimum p-value)
mq2f <- update(mq2f, .~. +x11)

# New iteration
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Add x10 (lowest p-value)
mq2f <- update(mq2f, .~. +x10)

# Final iteration
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# No further significant variables present.
# IDENTIFIED MODEL:
# lm(formula = y ~ x6 + x12 + x9 + x11 + x10)
# Summary information:
summary(mq2f)

# 3rd Approach: P-VALUE METHOD "MIXED"
# • The procedure starts with the forward method considering the null model and
# iteratively adds the variable with a significant effect that has the smallest p-value.
# • If after adding significant effect variables, others in the
# model become non-significant, they are removed.
# • The procedure ends when there are no more variables to add or remove
# based on the p-value.

# Setting the starting model based on the previous "FORWARD" strategy
# (evaluating if adding a variable makes another one non-significant)
mq3f <- update(mq0, .~. +x6 + x12)
summary(mq3f)
# The significance of x6 is slightly reduced, but sufficient to keep
# both.
# Adding the third identified variable:
mq3f <- update(mq3f, .~. +x9)
summary(mq3f)

# The significance is not altered.
# Adding the fourth identified significant variable
mq3f <- update(mq3f, .~. +x11)
summary(mq3f)

# The significance of all variables has now increased
# Adding the fifth significant variable
mq3f <- update(mq3f, .~. +x10)
summary(mq3f)

# Now x9 becomes non-significant, removing it
mq3f <- update(mq3f, .~. -x9)
summary(mq3f)

# Since there is a difference from the previous "FORWARD" procedure, now checking
# for the presence of other significant variables
mq3 <- update(mq3f, .~. +x1)
summary(mq3)

mq3 <- update(mq3f, .~. +x2)
summary(mq3)

mq3 <- update(mq3f, .~. +x3)
summary(mq3)

mq3 <- update(mq3f, .~. +x4)
summary(mq3)

mq3 <- update(mq3f, .~. +x5)
summary(mq3)

mq3 <- update(mq3f, .~. +x7)
summary(mq3)

mq3 <- update(mq3f, .~. +x8)
summary(mq3)

mq3 <- update(mq3f, .~. +x13)
summary(mq3)

mq3 <- update(mq3f, .~. +x14)
summary(mq3)

mq3 <- update(mq3f, .~. +x15)
summary(mq3)

mq3 <- update(mq3f, .~. +x16)
summary(mq3)

mq3 <- update(mq3f, .~. +x17)
summary(mq3)

mq3 <- update(mq3f, .~. +x18)
summary(mq3)

# NO FURTHER VARIABLES TO ADD/REMOVE.
# IDENTIFIED MODEL:
# lm(formula = y ~ x6 + x12 + x11 + x10)
# Summary information:
summary(mq3f)

# 4th Approach: LIKELIHOOD FUNCTIONS COMPARISON (FORWARD WITH K=0)
# Starting from the null model
forw_lik <- step(mq0, scope=formula(mq), direction="forward", k=0)
# ALWAYS CHOOSES THE COMPLETE MODEL:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10 + x5 + x15 + x7 + x2 + 
# x1 + x3 + x13 + x4 + x17 + x16 + x8
# Summary information
summary(forw_lik)

# 5th Approach: AIC PENALIZATION CRITERION (FORWARD WITH K=2)
# [Akaike Information Criterion:]
# The penalization term used is penAIC(d) = 2|d|, where given a model Fd(θ),
# the information index is AICd = 2|d| − ℓ(ˆθd; X).[estimator of theta in d]
# The best model within the explored class of models is the one with
# the minimum AICd value.
forw_aic <- step(mq0, scope=formula(mq), direction="forward", k=2)
# CHOOSES THE MODEL:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# Summary information
summary(forw_aic)

# 6th Approach: BIC PENALIZATION CRITERION (FORWARD WITH k=log(length(y)))
# [Bayesian Information Criterion]
# Penalization term: penBIC(d) = |d| log n.
# The criterion suggests choosing the model Fd(θ; X) with
# minimum BICd = |d| log n − ℓ(ˆθd; X).
forw_bic <- step(mq0, scope=formula(mq), direction="forward", k=log(length(y)))
# CHOOSES THE MODEL:
# y ~ x12 + x9 + x18 + x14 + x11
# Summary information
summary(forw_bic)

# 7th Approach: LIKELIHOOD FUNCTIONS COMPARISON (BOTH WITH K=0)
# STARTING FROM THE COMPLETE MODEL
both_lik <- step(mq, scope=formula(mq), direction="both", k=0)
# ALWAYS CHOOSES THE COMPLETE MODEL:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18
# Summary information
summary(both_lik)

# 8th Approach: AIC (BOTH WITH K=2)
both_aic <- step(mq, scope=formula(mq), direction="both", k=2)
# CHOOSES THE MODEL:
# y ~ x6 + x10 + x12 + x14 + x18
# Summary information
summary(both_aic)

# 9th Approach: BIC (BOTH WITH K=ln(length(y)))
both_bic <- step(mq, scope=formula(mq), direction="both", k=log(length(y)))
# CHOOSES THE MODEL:
# y ~ x6 + x10 + x12 + x14 + x18
# Summary information
summary(both_bic)

# 10th Approach: LIKELIHOOD FUNCTIONS COMPARISON (BACKWARD WITH K=0)
# STARTING FROM THE COMPLETE MODEL
back_lik <- step(mq, scope=formula(mq), direction="backward", k=0)
# ALWAYS CHOOSES THE COMPLETE MODEL:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18
# Summary information
summary(back_lik)

# 11th Approach: AIC (BOTH WITH K=2)
back_aic <- step(mq, scope=formula(mq), direction="backward", k=2)
# CHOOSES THE MODEL:
# y ~ x6 + x10 + x12 + x14 + x18
# Summary information
summary(back_aic)

# 12th Approach: BIC (BOTH WITH K=ln(length(y)))
back_bic <- step(mq, scope=formula(mq), direction="backward", k=log(length(y)))
# CHOOSES THE MODEL:
# y ~ x6 + x10 + x12 + x14 + x18
# Summary information
summary(back_bic)

# IDENTIFIED MODELS:
# BACKWARD (p-value): y ~ x6 + x10 + x12 + x14 + x18 [All extremely significant]
# FORWARD (p-value): y ~ x6 + x12 + x9 + x11 + x10 [All extremely significant except x9]
# MIXED (p-value): y ~ x6 + x12 + x11 + x10 [All extremely significant]
# FORWARD (LIKELIHOODS COMPARISON): y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10 + x5 + x15 + x7 + x2 + 
# x1 + x3 + x13 + x4 + x17 + x16 + x8
# FORWARD (AIC): y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# FORWARD (BIC): y ~ x12 + x9 + x18 + x14 + x11
# BOTH (LIKELIHOODS COMPARISON): y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18
# BOTH (AIC): y ~ x6 + x10 + x12 + x14 + x18
# BOTH (BIC): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (LIKELIHOODS COMPARISON): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (AIC): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (BIC): y ~ x6 + x10 + x12 + x14 + x18

# POTENTIALLY VALID MODELS:
# EXCLUDING COMPLETE AND DUPLICATE MODELS:
# A - 6 occurrences: y ~ x6 + x10 + x12 + x14 + x18
# B - 1 occurrence: y ~ x6 + x12 + x9 + x11 + x10
# C - 1 occurrence: y ~ x6 + x12 + x11 + x10
# D - 1 occurrence: y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# E - 1 occurrence: y ~ x12 + x9 + x18 + x14 + x11

# A
a <- lm(y ~ x6 + x10 + x12 + x14 + x18)
summary(a)
# B
b <- lm(y ~ x6 + x12 + x9 + x11 + x10)
summary(b)
# C
c <- lm(y ~ x6 + x12 + x11 + x10)
summary(c)
# D
d <- lm(y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10)
summary(d)
# E
e <- lm(y ~ x12 + x9 + x18 + x14 + x11)
summary(e)

# The most significant models are A and C (based on the p-values)
# CHOSEN FINAL MODEL:
# A with 6 occurrences: y ~ x6 + x10 + x12 + x14 + x18
summary(a)

# DEFINITION OF THE IDENTIFIED MODEL:
mq <- lm(y ~ x6 + x10 + x12 + x14 + x18)
print(summary(mq))

# PHASE 5:
# PRELIMINARY ANALYSIS OF THE MODEL AND RESIDUALS:

# LIST OF SIGNIFICANT VARIABLES FOR CENTRAL VELOCITY DISPERSION (Km/s):
# Mv: Absolute magnitude
# log.t: Central relaxation time (years)
# V.esc: Central escape velocity (Km/s)
# E.B.V: Color excess (Magnitude)
# CSBt: Central surface brightness (Magnitude per square arcsecond)

# VARIABLE ANALYSIS:
# ALL VARIABLES ARE EXTREMELY SIGNIFICANT
# EFFECT OF VARIABLES ON CENTRAL VELOCITY DISPERSION:
# Mv: an increase in magnitude of one unit leads to an increase of approximately 0.536888 Km/s
# log.t: one year of central relaxation on a logarithmic scale leads to an increase of 1.346157 Km/s
# V.esc: an increase in escape velocity of 1 Km/s results in an increase of 0.233623 Km/s
# E.B.V: an increase of one unit in the magnitude of color excess results in an increase of 1.541982 Km/s
# CSBt: a one-unit increase in central surface brightness results in a decrease of 0.506816 Km/s

# ANALYSIS OF MODEL RESULTS
# STANDARD ERROR ESTIMATE: 0.1636 with 107 degrees of freedom [being relatively
# small, indicates good precision of the estimates.]
# DETERMINATION COEFFICIENT: 0.9977 [being very high, indicates that the
# model almost completely explains the variance of the dependent variable y.]
# F-TEST: EXTREMELY SIGNIFICANT [9661 out of 5 and 107 DF]
# Residuals are well distributed, with the median close to zero.
# A very high F-value and a very low p-value (as indicated in the
# results provided, p < 2.2e-16) reject the null hypothesis that the model with
# only the intercept term is equally good at explaining the variability
# in the dependent variable.]:
# IN THIS CASE, THE MODEL SHOWS AN EXCELLENT FIT!

# CREATING THE MODEL GRAPH:
plot(mq)

# The residuals appear to have an approximately normal distribution.
# The graph shows the presence of some extreme points
# that potentially interfere with the model (outliers).
# ANALYSIS OF POTENTIAL OUTLIERS:
boxplot(mq$residuals)

# GRAPH ANALYSIS:
# Residuals are centered: The median near zero indicates that the regression model is
# not biased either high or low.
# The concentration of data near the median and symmetry suggest
# that most of the data follows the identified model.
# However, there are potential outliers: Points outside the whiskers (at the extremes)

# Calculation of standardized residuals
std_res <- rstandard(mq)
# Identification of outliers
# The number "2" is a threshold conventionally chosen for the normal
# (or Gaussian) distribution. In a standardized normal distribution:
# About 95% of the data lies within 2 standard deviations from the mean.
outliers <- which(abs(std_res) > 2)
# Removing outliers from the dataset
clean_dat <- dat[-outliers, ]
# Verification of outlier removal
print(nrow(dat))
print(nrow(clean_dat))
# Updating the model without outliers
clean_mq <- lm(y ~ x6 + x10 + x12 + x14 + x18, data=clean_dat)
# Graph of residuals with and without outliers
par(mfrow=c(1,2))
plot(mq$fitted.values, mq$residuals, main="With Outliers")
abline(h=0, col="red")
plot(clean_mq$fitted.values, clean_mq$residuals, main="Without Outliers")
abline(h=0, col="blue")
# Resetting graph creation settings
par(mfrow=c(1,1))

# GRAPH ANALYSIS:
# Outliers do not seem to have a significant impact on the model, as the graphs
# are practically identical. This indicates that the model is quite
# robust and has a high tolerance for anomalous values.
# I will keep the model unchanged.

# DATA RESULTING FROM THE USE OF A GLM (Generalized Linear Model)
# DEFINITION OF THE IDENTIFIED MODEL:
mqg <- glm(y ~ x6 + x10 + x12 + x14 + x18)
print(summary(mqg))

# THE IDENTIFIED DISTRIBUTION FAMILY IS GAUSSIAN
# WITH IDENTITY LINK FUNCTION
print(mqg$family)

# The glm model, using the same formula as the linear model (y ~ x6 + x10 + x12 + x14 + x18),
# suggests similar results in terms of variable significance,
# as indicated by the coefficients and p-values. This indicates that all the selected
# variables are statistically significant in the model.
# The main difference lies in the use of glm, which can be adapted to
# model a wider range of residual distributions compared to lm.
# The model behaves similarly to lm for normally distributed data.
# The dispersion parameter and AIC value also indicate good model fit to the data.

# EVALUATION OF THE EFFECT OF INDIVIDUAL VARIABLES ON CENTRAL
# VELOCITY DISPERSION:

# ABSOLUTE MAGNITUDE
mqgx6 <- glm(y ~ x6)
summary(mqgx6)

# The variable's high significance is confirmed.
# An increase in magnitude level leads to a reduction in velocity
# dispersion of 2.2190 Km/s

# CENTRAL RELAXATION TIME:
mqgx10 <- glm(y ~ x10)
summary(mqgx10)

# High significance is confirmed again, albeit slightly
# reduced compared to the model containing all variables.
# Each year of relaxation results in a velocity reduction of 1.0902 Km/s

# CENTRAL ESCAPE VELOCITY:
mqgx12 <- glm(y ~ x12)
summary(mqgx12)

# High significance is confirmed.
# An increase in escape velocity of 1 Km/s corresponds to an increase in
# velocity dispersion of 0.23609 Km/s

# COLOR EXCESS MAGNITUDE
mqgx14 <- glm(y ~ x14)
summary(mqgx14)

# Significant, but reduced compared to the model containing
# all variables.
# An increase of one unit in color excess magnitude results in an
# increase in velocity dispersion of 1.80018 Km/s

# CENTRAL SURFACE BRIGHTNESS:
mqgx18 <- glm(y ~ x18)
summary(mqgx18)

# Highly significant.
# An increase in central surface brightness by one unit (magnitude per square arcsecond)
# leads to a reduction in velocity dispersion of 1.1231 Km/s

# PHASE 6:
# CORRELATION ANALYSIS AMONG THE MODEL VARIABLES:

# CREATING CORRELATION GRAPHS AMONG THE MODEL VARIABLES
plot(x6, x10)
plot(x6, x12)
plot(x6, x14)
plot(x6, x18)
plot(x10, x12)
plot(x10, x14)
plot(x10, x18)
plot(x12, x14)
plot(x12, x18)
plot(x14, x18)

# GRAPH ANALYSIS:
# POSSIBLE IDENTIFIED CORRELATIONS:
# x6-x12: x12 decreases as x6 increases, with a negative exponential trend
# x6-x18: Increase linearly
# x10-x18: Increase linearly
# x12-x18: x18 decreases as x12 increases, with a negative exponential trend

# EVALUATION OF MORE COMPLEX REGRESSION MODELS CONSIDERING
# POTENTIAL CORRELATIONS:
mqgx6x12 <- glm(y ~ x6*x12 + x10 + x14 + x18)
summary(mqgx6x12)

# The p-value for the interaction term is 0.0769, which is above the
# threshold of 0.05. This suggests that while the individual variables are significant,
# the additional effect of their interaction is not statistically
# significant.

# CORRELATION INDEX
cor(x6, x12) # Indicates a strong negative correlation between the two

# Mv and CSBt
mqgx6x18 <- glm(y ~ x6*x18 + x10 + x12 + x14)
summary(mqgx6x18)

# The effect of absolute magnitude on velocity dispersion varies based on
# different levels of central surface brightness and vice versa.
# The interaction between these two measures is significant, and an increase in it leads to a
# reduction in velocity dispersion of 0.028592 Km/s

# CORRELATION INDEX:
cor(x6, x18) # confirms a positive correlation

# CONFIDENCE INTERVAL:
# With a test statistic (DEV) of 63.453 and a p-value very close to 0,
# a strong dependency between the two variables is highlighted
ci.dumpx6x18 <- ciTest(dat, set=c("Mv", "CSBt"))
print(ci.dumpx6x18)

# log.t and CSBt
mqgx10x18 <- glm(y ~ x10*x18 + x6 + x12 + x14)
summary(mqgx10x18)

# With a p-value of 0.00102, there is a significant dependency between central relaxation time and
# central surface brightness in the context of velocity dispersion.
# An increase in the interaction leads to a reduction in velocity dispersion
# of 0.035867 Km/s

# CORRELATION INDEX:
cor(x10, x18) # confirms a positive correlation

# CONFIDENCE INTERVAL:
# Shows a test statistic (DEV) of 34.811 with a p-value very close to 0,
# denoting a strong dependency between the two variables
ci.dumpx6x18 <- ciTest(dat, set=c("log.t", "CSBt"))
print(ci.dumpx6x18)

# V.esc and CSBt
mqgx12x18 <- glm(y ~ x12*x18 + x6 + x10 + x14)
summary(mqgx12x18)

# With a p-value of 2.53e-05, the interaction between escape velocity and central surface
# brightness is highly significant for velocity dispersion.
# An increase in the interaction leads to an increase in velocity dispersion
# of 0.03507 Km/s

# CORRELATION INDEX:
cor(x12, x18) # confirms a strong negative correlation

# CONFIDENCE INTERVAL:
# The test statistic (DEV) is 85.399 and a p-value very close to 0 suggest
# a strong dependency between the two variables
ci.dumpx12x18 <- ciTest(dat, set=c("V.esc", "CSBt"))
print(ci.dumpx12x18)

# CORRELATION RESULT ANALYSIS:
# It is evident that central surface brightness is influenced by absolute magnitude,
# central relaxation time, and central escape velocity!

# UPDATING THE MODEL EXPRESSING THE IDENTIFIED INTERACTIONS BETWEEN VARIABLES:
mqgf <- glm(y ~ x6*x18 + x10*x18 + x12*x18 + x14)
print(summary(mqgf))

# ANALYSIS OF THE NEW MODEL:
# The previous glm model, which does not include interaction terms, already
# shows good consistency with the data, as indicated by the low AIC and high
# significance of the coefficients.
# However, the addition of interaction terms has led to some improvements,
# as indicated by an even lower AIC in the second model.
# The interaction x12:x18 is not significant!

# PROCEEDING WITH THE REMOVAL OF SUPERFLUOUS INTERACTION
# REMOVING the interaction between x12 and x18
mqgf <- glm(y ~ x6*x18 + x10*x18 + x12 + x14)
print(summary(mqgf))

# The resulting model is further improved and the significance of the
# variables and interactions has increased.
# There is better model quality in terms of balance between fit
# and complexity (More balanced)

# CORRELATION GRAPHS BETWEEN VARIABLES AND CENTRAL VELOCITY DISPERSION:
plot(x6, y)
plot(x10, y)
plot(x12, y)
plot(x14, y)
plot(x18, y)

# FROM THESE GRAPHS, IT IS IMMEDIATELY VISIBLE THAT THE CENTRAL ESCAPE VELOCITY
# IS CLOSELY CORRELATED TO THE CENTRAL VELOCITY DISPERSION
# AS THE TWO QUANTITIES INCREASE LINEARLY

# CORRELATION INDEX:
cor(x12, y) # Extreme positive correlation

# PHASE 7:
# MULTIVARIATE ANALYSIS BASED ON GRAPHICAL MODELS:

# Searching for a formula for the independent model (BIC Forward), based on the
# general linear model identified as final
m.dumping.ind <- cmod(~.^1, marginal =c("Mv","log.t", "V.esc","E.B.V", "CSBt"), data = dat, fit = TRUE)
m.dumping <- stepwise(m.dumping.ind, direction = "forward", k=log(sum(dat))) # BIC forward
plot(m.dumping)
print(formula(m.dumping))
print(ciTest(dat, set = c("Mv","log.t", "V.esc","E.B.V", "CSBt")))

set.frame <- data.frame(
  Mv = dat$Mv,
  log.t = dat$log.t,
  V.esc = dat$V.esc,
  E.B.V = dat$E.B.V,
  CSBt = dat$CSBt
)

print(cor(set.frame))

# Based on the correlation matrix, removing less significant dependencies (values close to zero)
# Discarding weak correlations |corr| < 0.5
m.dumping <- update(m.dumping, list(dedge=~Mv * E.B.V + log.t * Mv + V.esc * log.t + E.B.V * log.t + V.esc * E.B.V + E.B.V * CSBt))
plot(m.dumping)
print(formula(m.dumping))
# Found formula:
# ~E.B.V + log.t * CSBt + Mv * CSBt * V.esc

# Creating a Bayesian network
cad.bn <- hc(set.frame)
plot(cad.bn)

# In this case too, the same correlations between
# the variables of the final model with their respective directions have been identified.
