#
# Exercises STK4900
# part 13
# Day 5 - week 1
#

# Predicting water salinity

# QUESTION  a)

 

# Read the data into a dataframe, give names to the variables, and inspect the data:
salinity=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/exer5.dat")
names(salinity)=c("salt","saltprev","trend","discharge")
salinity


# Overview
summary(salinity)
plot(salinity)

# Do linear regression with all three covariates and inspect the results:
lmfull=lm(salt~saltprev+trend+discharge, data=salinity)
summary(lmfull)

# QUESTION  b)

 

# Extract fitted values and residuals, and give a summary of the residuals:

saltfit=lmfull$fit
saltres= lmfull$res
summary(saltres)
boxplot(saltres)

# Residuals are (y_real - y_model) and are scattered around 0, should be 
# 50% above and 50% below approximately

# QUESTION  c)

 

# We will make various plots of the residuals

 

# (i) Checking normality

# Histogram and Q-Q plot (make one plot at a time)
hist(saltres)
qqnorm(saltres); qqline(saltres)    # alternative command:  plot(lmfull,2)
 
# What do the plots tell you?
# Histogram: distribution of residuals, should be gaussian with mean 0
#		shows slightly positive curve




 

# (ii) Checking homoscedasticy 
# Plot of residuals versus fitted values:
plot(saltfit, saltres, xlab="Fitted values", ylab="Residuals")

# Alternative plots:
plot(lmfull,1)
plot(lmfull,3)

# slightly negative residuals

 
