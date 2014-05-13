#
# Exercises STK4900
# part 18
# Day 2 - week 2
#

# A second look at logistic regression

# Data from Western Collaborative Group Study (WCGS)
wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/wcgs.txt",
sep="\t",header=T,na.strings=".")
summary(wcgs)

# a) Logistic regression model with smoke as covariate
fit.smoke=glm(chd69~smoke,data=wcgs,family=binomial)
summary(fit.smoke)
	# Significant effect of smoking on the risk of devel CHD?
	# z-value = 4.71, null hypothesis can be rejected - smoking can cause CHD

	# 95 % CI for reg coeff for smoking:
cbind(0.6299, 0.1337, 0.6299 - 1.96*0.1337,  0.6299 + 1.96*0.1337)

	# Odds ratio
exp(0.6299)
	# odds ratio CI
cbind(exp(0.6299), exp(0.6299 - 1.96*0.1337),  exp(0.6299 + 1.96*0.1337))

# b) introduce function

expcoef=function(glmobj)
{ 
regtab=summary(glmobj)$coef 
expcoef=exp(regtab[,1]) 
lower=expcoef*exp(-1.96*regtab[,2]) 
upper=expcoef*exp(1.96*regtab[,2]) 
cbind(expcoef,lower,upper)
}

expcoef(fit.smoke)
	# same results

# c) Logistic regression to study the effect of age for the risk of devel CHD
fit.age=glm(chd69~age,data=wcgs,family=binomial)
summary(fit.age)
	
	# Use expcoef to find OR for ONE-YEAR INCREASE in AGE w/ 95 % CI
expcoef(fit.age)

	# gives a OR of 1.077 for increasing age compared to prior year


# d) Odds ratio: ten-year increase in age

fit.age10=glm(chd69~I(age/10),data=wcgs,family=binomial)
summary(fit.age10)

	# Comment: does not change intercept nor regression coeff.
	# same z-value.

expcoef(fit.age10)
	# gives higher OR
exp(0.7442) 	# Ten year increase
exp(0.7442/10)	# one year increase
