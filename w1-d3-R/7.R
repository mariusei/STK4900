#
# Exercises STK4900
# part 7
# Day 3 - week 1
#

# understanding correlation

library(MASS)

# a)
# Generate 25 obs (x,y) from a BIVARIATE NORMAL DISTRIBUTION
# 		w/ correlation 0.30

n=2500
rho=-0.70
m=matrix(c(0,0),nrow=2)
S=matrix(c(1,rho,rho,1),nrow=2)
obs=mvrnorm(n,m,S)
x=obs[,1]
y=obs[,2]
cor(x,y) #!!!!!! IMPORTANT - Changes even though rho is fixed
plot(x,y)

