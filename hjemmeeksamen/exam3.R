# PROJECT EXAM 
# STK4900   -   Spring 2014

### PROBLEM 3 ###

# Load data:
wheeze<-read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v14/wheeze.txt",h=T)

smoking     = wheeze$smoking            # binary, yes=1, no=0
age         = wheeze$age                # in years, of childe
wheezing    = wheeze$wheeze             # binary, yes=1, no=0

# 3a) Proportion that wheeze and not
n_ys   = sum(smoking==1)
n_ns   = sum(smoking==0)
p_ys   = sum(wheezing==1 &  smoking==1) / n_ys 
p_ns   = sum(wheezing==1 &  smoking==0) / n_ns 

# Significant difference of wheezing in the two groups?
#   H0: p_ys = p_ns

p_diff        = p_ys - p_ns
p_diff.stderr = sqrt( (p_ys * ( 1 - p_ys ) )/n_ys  +  ( p_ns * (1 - p_ns) )/n_ns ) 
lower         = p_diff - 1.96 * p_diff.stderr
upper         = p_diff + 1.96 * p_diff.stderr
p_diff.95CI   = cbind(p_diff,  p_diff.stderr, lower, upper)

# Standard error under null hypothesis
p_tot         = sum(wheezing==1) / (n_ys + n_ns)
p_diff.se0    = sqrt( p_tot*(1-p_tot)/n_ys + p_tot*(1-p_tot)/n_ns )
p_diff.z      = p_diff / p_diff.se0

# Relative risk and odds ratio?
# The relative risk is the ratio of the proportions: for and against
#   Here: ratio between p_ys and p_ns

RR = p_ys/p_ns
OR = p_ys/(1-p_ys) / ( p_ns/(1-p_ns) )

# PRINT RESULTS TO FILE
sink("res_ex3.txt", split=TRUE)
print("95% CI for wheezing difference:")
print(p_diff.95CI)
print("Proportion: wheeze w/ smoking mother")
print(p_ys)
print("Proportion: wheeze w/o smoking mother")
print(p_ns)

print("Total proportion that wheezes:")
print(p_tot)
print("Standard error under null hypothesis:")
print(p_diff.se0)
print("z-statistic")
print(p_diff.z)

print("Relative risk")
print(RR)
print("Odds ratio:")
print(OR)
sink()

### 3b) Analysing data using logistic regression
glm1        = glm(wheezing~factor(smoking), family=binomial)
glm2        = glm(wheezing~factor(smoking)+age, family=binomial)
glm1.beta1  = glm1$coefficient[2]
glm1.stderr = summary(glm1)$coefficients[2,2]
glm1.OR.lower = exp(glm1.beta1 - 1.96 * glm1.stderr)
glm1.OR.upper = exp(glm1.beta1 + 1.96 * glm1.stderr)
glm1.95CI   = cbind(exp(glm1.beta1), exp(glm1.stderr), 
                    glm1.OR.lower, glm1.OR.upper)

# Test whether a model with two covariates is better than one?
glm1glm2    = anova(glm1, glm2, test="Chisq")

# Print results to file
sink("res_ex3b.txt", split=TRUE)
print("Logistic model with only one covariate")
print(summary(glm1))
print("Odds ratio and CI for ONE COVARIATE")
print(glm1.95CI)
print("Logistic model with two covariates")
print(summary(glm2))
sink()
sink("res_ex3b_anova.txt", split=TRUE)
print("Differences in models: one covariate vs two covariates")
print(glm1glm2)

sink()
