# PROJECT EXAM 
# STK4900   -   Spring 2014

### PROBLEM 2 ###
source("utils_STK4900.R")

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


### 2c) Poisson model with interaction and quadratic terms
fit.hugemodel = glm(numvisit~reform+age+badh+loginc+reform:age+reform:badh+reform:loginc+reform:age+age:badh+age:loginc+badh:loginc)


model.step1 = lapply(c(
        numvisit~reform+age+badh+loginc,
        numvisit~reform+age+badh+loginc+reform:age,
        numvisit~reform+age+badh+loginc+reform:badh,
        numvisit~reform+age+badh+loginc+reform:loginc,
        numvisit~reform+age+badh+loginc+age:badh,
        numvisit~reform+age+badh+loginc+age:loginc,
        numvisit~reform+age+badh+loginc+badh:loginc,
        numvisit~reform+age+badh+loginc+I(age^2),
        numvisit~reform+age+badh+loginc+I(loginc^2)
                 ), sumry_poisson)

model.step2 = lapply(c(
        numvisit~reform+age+badh+loginc+I(loginc^2),
        numvisit~reform+age+badh+loginc+I(loginc^2)+reform:age,
        numvisit~reform+age+badh+loginc+I(loginc^2)+reform:badh,
        numvisit~reform+age+badh+loginc+I(loginc^2)+reform:loginc,
        numvisit~reform+age+badh+loginc+I(loginc^2)+age:badh,
        numvisit~reform+age+badh+loginc+I(loginc^2)+age:loginc,
        numvisit~reform+age+badh+loginc+I(loginc^2)+badh:loginc,
        numvisit~reform+age+badh+loginc+I(loginc^2)+I(age^2)
                 ), sumry_poisson)

model.step1.deviances = NULL
model.step2.deviances = NULL

for (i in 1:length(model.step1)) {
    model.step1.deviances[i] = model.step1[[i]]$deviance 
}
for (i in 1:length(model.step2)) {
    model.step2.deviances[i] = model.step2[[i]]$deviance 
}

# What model minimises the deviance?
dev1.H0  = model.step1.deviances[1]
dev1.min = min(model.step1.deviances)
dev1.minindex = which(model.step1.deviances == dev1.min)
G1 = dev1.H0 - dev1.min

dev2.H0  = model.step2.deviances[1]
dev2.min = min(model.step2.deviances)
dev2.minindex = which(model.step2.deviances == dev2.min)
G2 = dev2.H0 - dev2.min

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
sink("res_ex2c-model1.txt", split=TRUE)
print("STEP 1: model no, deviance")
for (i in 1:length(model.step1)) {
    print(paste0(i, " ",  model.step1.deviances[i]))
}
print("G statistic for smallest deviance? STEP 1")
print(paste0("i = ", dev1.minindex, " has G = ", G1))
print(anova(model.step1[[1]], model.step1[[dev1.minindex]], test="Chisq"))

sink()
sink("res_ex2c-model2.txt", split=TRUE)
print("STEP 2: model no, deviance")
for (i in 1:length(model.step2)) {
    print(paste0(i, " ",  model.step2.deviances[i]))
}

print("G statistic for smallest deviance? STEP 2")
print(paste0("i = ", dev2.minindex, " has G = ", G2))
print(anova(model.step1[[dev1.minindex]], model.step2[[dev2.minindex]], test="Chisq"))

sink()
sink("res_ex2c.txt", split=TRUE)
print("Best fit model summary")
print(summary(model.step1[[9]]))

sink()
