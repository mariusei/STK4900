#
# Exercises STK4900
# part 12
# Day 5 - week 1
#

# Three way analysis of variance

gun=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/gun.dat", col.names=c("method","phys","team","rounds"))
gun

# Correlations
cor(gun)

# Strongly 	correlated: rounds and method, negatively
#	?	correlated: rounds and phys
# not very?	correlated: rounds and team

# correlation between covariates: method, phys and team are all zero
#	-> these are apparently uncorrelated
plot(gun$method, gun$phys)

# B; C; D:
# Define the covariates as factors (categorial covariates):
gun$method	= factor(gun$method)
gun$phys	= factor(gun$phys)
gun$team	= factor(gun$team)
plot(gun$method, gun$phys) # the data have been transformed

# Fit a model w/ main effects and niteractions and write down the anova table
gfit 		= lm(rounds~method*phys*team, data=gun)
anova(gfit)
summary(gfit)

# ANOVA table interpretation
#	Gives information on the variance of data dependent on multiple
#	covariates. 

#	F value is dependent on two or more covariates and 
#	gives the groupwise probability of a group having impact on
#	the response

# The t-test gives information for comparision between two groups
# to see if there is a significant correlation between these two, expressed
# either as Pearson's r (showing degree of correlation) or 
# Student's t (giving the probability that the null hypothesis is true,
# that is, the two groups are completely uncorrelated giving a slope = 0 in
# a linear model or Pearson's = 0).

#  Important results
# The Linear Model results gives that the baseline is the first group,
# 1st method and first physiology and the
# other groups are given binary covariates. The model has also higher order 
# cross terms. The intercept consists of the outcome given group 1, method 1
# and physiology one and all other slopes gives CHANGES COMPARED TO THIS.