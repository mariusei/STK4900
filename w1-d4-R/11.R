#
# Exercises STK4900
# part 11
# Day 4 - week 1
#

# multiple linear regression
# Heart and Estrogen/Progestin Study


# HERS DATA
hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/hers.txt",sep="\t",header=T,na.strings=".")

# Change in LDL: deltaLDL
hers$deltaLDL 	= hers$LDL1 - hers$LDL

# Centered LDL at baseline
#	> Subtracting the mean value 145 mg/dL
hers$cLDL = hers$LDL -145


# A Fit a linear model: change in LDL as response (y) and
#	hormone therapy HT and baseline LDL not centered as covariates (x)

fit.a = lm(deltaLDL~HT+LDL, data=hers)
summary(fit.a)

# Fitted model has t values for HT and LDL showing that the NULL HYPOTHESIS is
# INSIGNIFICANT.
# Linear model: change in LDL = -15,45*HT -0,3876*LDL + 51,30

# B Fit a model w HT and CENTERED LDL at baseline as covariates
fit.b = lm(deltaLDL~HT+cLDL, data=hers)
summary(fit.b)