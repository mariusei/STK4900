#
# Exercises STK4900
# part 17
# Day 1 - week 2
#

# A first look at logistic regression

# Data from Western Collaborative Group Study (WCGS)
wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/wcgs.txt",
sep="\t",header=T,na.strings=".")
summary(wcgs)

# a) Smoking affects likelihood of obtaining CHD?
table(wcgs$smoke, wcgs$chd69)
# 1554 non-smokers => 98 got CHD
# 1343     smokers => 159 got CHD

p1 = sum(wcgs$chd69 & wcgs$smoke==1) / sum(wcgs$smoke)
p0 = sum(wcgs$chd69 & wcgs$smoke==0) / sum(wcgs$smoke==0)
	# Relative risk
RR = p1/p0
	# Odds ratio
OR = (p1/(1-p1)) / (p0/(1-p0))
cbind(p1,p0, RR, OR)
	# RR is approx 2 (1,784) - twice as likely, and
	# OR is approx 2 (1,878) - twice as large odds compared to p0
	# 	For small p - RR approx OR


# b) Logistic regression model with smoke as covariate
fit.smoke = glm(chd69~smoke, data=wcgs, family=binomial)
print(fit.smoke)

	# Odds ratio correpsonding to a one unit's increase in a covariate
	# Compute odds ratio for CHD for a smoker compared to a non-smoker?
	# Odds ratio is exp(beta1)
exp(0.6299)
OR
	# which is approximately equal to OR, or that changing category
	# changes the odds corresponding

# c)
# We then use logistic regression to study the effect of age 
# (at entry to the study) for the risk of developing CHD:
fit.age=glm(chd69~age,data=wcgs,family=binomial)
print(fit.age)

# From a fitted logistic model we may estimate the odds ratio 
# corresponding to a given increase in a covariate (cf. slide 25).
# Compute the odds ratio for CHD for a one-year and a ten-year
# increase in age.
exp(

# Check that you get the same results as on pages 162-163 in the text book byVittinghoff et al.


