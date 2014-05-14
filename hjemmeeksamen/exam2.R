# PROJECT EXAM 
# STK4900   -   Spring 2014

### PROBLEM 2 ###

# Load data:
drvisits<-read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v14/drvisits.txt",h=T)

numvisit = drvisits$numvisit        # no of doc visits during three months
age      = drvisits$age             # age in years
educ     = drvisits$educ            # education in years
married  = factor(drvisits$married)    # yes/no
badh     = factor(drvisits$badh)    # bad health = true/false
loginc   = drvisits$loginc          # Log of income
reform   = factor(drvisits$reform)  # surveyed prior/after health reform?


### 2a) Poisson model, only health reform
fit.reform = glm(numvisit~reform, family=poisson)

# Find rate ratio 95 % CI 
expcoef=function(glmobj)
{
regtab=summary(glmobj)$coef
expcoef=exp(regtab[,1])
lower=expcoef*exp(-1.96*regtab[,2])
upper=expcoef*exp(1.96*regtab[,2])
cbind(expcoef,lower,upper)
}

fit.reform.RR.95CI = expcoef(fit.reform)


### 2b) Poisson model using all covariates
fit.fullmodel = glm(numvisit~reform+age+educ+married+badh+loginc, family=poisson)
fit.fullmodel.RR.95CI = expcoef(fit.fullmodel)

# What effect has being 60 years of age on the doctoral visitation rate?
age60 = fit.fullmodel.RR.95CI[3,1]  ^ ( 60 - min(age) )

# What is the impact on the visitation rate by being richest?
richest = fit.fullmodel.RR.95CI[7,1] ^ ( max(loginc) - min(loginc) )

# Save results to file
sink("res_ex2a.txt", split=TRUE)
print("2A: POISSON MODEL: only health reform covariate")
print(summary(fit.reform))
print("2A: 95% CI FOR THE RATE RATIO:")
print(fit.reform.RR.95CI)
sink()

sink("res_ex2b.txt", split=TRUE)
print("2B: POISSON MODEL: using all covariates")
print(summary(fit.fullmodel))
print("2B: POISSON MODEL: all covariates -- RR 95 % CI:")
print(fit.fullmodel.RR.95CI)
print("2B: What impact has it to be 60 years of age?")
print(age60)
print("2B: What is the RR for the household with the HIGHEST income to the lowest?")
print(richest)


sink()
