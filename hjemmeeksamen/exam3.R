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
glm1.beta1  = fit.glm1$coefficient[2]
glm1.stderr = summary(fit.glm)$coefficients[2,2]
glm1.OR.lower = exp(glm1.beta1 - 1.96 * glm1.stderr)
glm1.OR.upper = exp(glm1.beta1 + 1.96 * glm1.stderr)
glm1.95CI   = cbind(exp(glm1.beta1), exp(glm1.stderr), 
                    glm1.OR.lower, glm1.OR.upper)


# Print results to file
sink("res_ex3b.txt", split=TRUE)
print("Logistic model with only one covariate")
print(summary(glm1))
print("Odds ratio and CI for ONE COVARIATE")
print(glm1.95CI)
print("Logistic model with two covariates")
print(summary(glm2))


## Relation between CH20 and AIR tightness
#par(ps = 22, cex = 2, cex.main = 2)
#pdf('1a1-CH2O_air.pdf')
#plot(AIR, CH2O, ylab='CH2O concentration [ppm]', xlab='Air tightness [0,10]'
#     ,col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
## Relation between CH20 concentration and presence of UFFI
#pdf('1a2-CH2O_UFFI.pdf')
#plot(UFFI, CH2O, ylab='CH2O concentration [ppm]', xlab='UFFI present'
#     ,col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
#
## Fit a linear model?
#fit1 = lm(CH2O~AIR)
#fit2 = lm(CH2O~UFFI)
#
##print(summary(fit1))
##print(summary(fit2))
#
#### 1b) Multiple regression
#fit3 = lm(CH2O~AIR+UFFI)
#fit4 = lm(CH2O~AIR+UFFI+AIR:UFFI)
#fit5 = lm(CH2O~AIR+UFFI+AIR:UFFI+I(AIR^2))
#
#print(summary(fit3))
#print(summary(fit4))
#print(summary(fit5))
#
#sumry = function(arg){
#    lm(arg, x=T,y=T)
#}
#
#cv.R2=function(lmobj,y=lmobj$y,x=lmobj$x)
#{
#   a=t(x)%*%x
#   d=diag(1/eigen(a)$values)
#   e=eigen(a)$vector
#   inv.a=e %*% d %*% t(e)
#   v=diag(x%*%inv.a%*%t(x))
#   SSkryss=sum((lmobj$res/(1-v))^2)
#   SStot=sum((y-mean(y))^2)
#   R2kryss=1-SSkryss/SStot
#   R2kryss
#}
#
#model = lapply(c(
#        CH2O~UFFI,
#        CH2O~AIR,
#        CH2O~AIR+UFFI,
#        CH2O~AIR+UFFI+AIR:UFFI,
#        CH2O~AIR+UFFI+AIR:UFFI+I(AIR^2)
#                 ), sumry)
#
#model.cvR2s = NULL 
#model.R2s   = NULL 
#model.adjR2s= NULL 
#
#for (i in 1:length(model)) {
#    model.cvR2s[i]  = cv.R2(model[[i]])
#    model.R2s[i]    = summary(model[[i]])$r.squared
#    model.adjR2s[i] = summary(model[[i]])$adj.r.squared
#}
#for (i in 1:length(model)) {
#    print(i)
#    print(model.adjR2s[i])
#}
#
#pdf("1a3-model_crossvalR2.pdf")
#plot(model.cvR2s,
#     xlab="Model number", ylab="R2",
#     ylim=c(0.0,1),
#     type='o', col="red", pch=19, lty=1,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#lines(model.R2s, type='o', col='blue', lty=2, pch=12,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#lines(model.adjR2s, type='o', lty=1,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
## Checking model 3, as this is the one where R2_CV is highest
#pdf('1c1-model3_residuals.pdf')
#plot(lm(CH2O~AIR+UFFI),1, 
#     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#pdf('1c2-model3_stdresiduals.pdf')
#plot(lm(CH2O~AIR+UFFI),3, 
#     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()



#
#weight = bears$WEIGHT
#length = bears$LENGTH
#chest  = bears$CHEST
#
#par(ps = 22, cex = 2, cex.main = 2)
#
## Boxplot: weight, length, chest:
#pdf('a1-weight_length_chest.pdf')
#par(mfrow=c(1,3))
#boxplot(weight, ylab="Weight [pounds]",
#        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
#boxplot(length, ylab="Length [inches]",
#        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
#boxplot(chest, ylab="Chest circumference [inches]",
#        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
## Scatterplot: length vs weight and chest vs weight
#pdf('a2-weight_length.pdf')
#plot(weight, length, xlab="Weight [lbs]", ylab="Length [in]",
#     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
#pdf('a3-weight_chest.pdf')
#plot(weight, chest, xlab="Weight [lbs]", ylab="Chest circumference [in]",
#     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
## Are there any correlations?
#print("Pearson cor. 1) Weight, length, 2) Weight, chest circum")
#print(cor(weight, length))
#print(cor(weight, chest))
#print("Spearman cor. 1) Weight, length, 2) Weight, chest circum")
#print(cor(weight, length, method="spearman"))
#print(cor(weight, chest, method="spearman"))
#
#### B) FIT LINEAR MODEL
#
#weightmod_1 = lm(weight~length+chest)
#weightmod_2 = lm(log(weight)~log(length)+log(chest))
#
#
#### C) PLOTS OF RESIDUALS
#
#pdf('c1-weightmod_1-residuals.pdf')
#plot(weightmod_1, 1,
#     col="blue", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#pdf('c2-weightmod_2-residuals.pdf')
#plot(weightmod_2, 1,
#     col="blue", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
#
#### D) REMOVE OUTLIER no 52
#
#bearsImp = bears[-52,]
#weightmod_3 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST), data=bearsImp)
#print(summary(weightmod_3))
#
#
#### E) USE ONLY OLD BEARS
#bearsOld = bearsImp[bearsImp$AGE>12,]
#weightmod_4 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST), data=bearsOld)
#print(summary(weightmod_4))
#
#
#### F) FIND EXPECTED WEIGHT FOR 65 in long, 40 in round BEAR
#print("Expected weight: ex f):")
#print(exp(-6.061)*65^(.9934)*40^(1.983))
#
#
#### G) USE ALL COVARIATES
#
#bearsOld$SEX    = factor(bearsOld$SEX)
#bearsOld$MONTH  = factor(bearsOld$MONTH)
#
#weightmod_5 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST)+log(NECK)+log(HEADWTH)+
#                 log(HEADLEN)+SEX+MONTH+log(AGE), data=bearsOld,
#                 x=T, y=T)
#sink("g_output.txt")
#print(summary(weightmod_5))
#sink()
#
#
#### H) CROSS-VALIDATED R SQUARED
#
#cv.R2=function(lmobj,y=lmobj$y,x=lmobj$x)
#{
#   a=t(x)%*%x
#   d=diag(1/eigen(a)$values)
#   e=eigen(a)$vector
#   inv.a=e %*% d %*% t(e)
#   v=diag(x%*%inv.a%*%t(x))
#   SSkryss=sum((lmobj$res/(1-v))^2)
#   SStot=sum((y-mean(y))^2)
#   R2kryss=1-SSkryss/SStot
#   R2kryss
#}
#
#print("Cross-validated R2 for huge dataset:")
#print(cv.R2(weightmod_5))
#
#
#### I) FORWARD SELECTION
#
#sumry = function(arg){
#    lm(arg, data=bearsOld, x=T,y=T)
#}
#
#model = lapply(c(
#        log(WEIGHT)~log(CHEST),
#        log(WEIGHT)~log(CHEST)+log(LENGTH),
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH),
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE),
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH,
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
#        log(HEADLEN),
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
#        log(HEADLEN)+log(NECK),
#        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
#        log(HEADLEN)+log(NECK)+SEX
#                 ), sumry)
#
#model.cvR2s = NULL 
#model.R2s   = NULL 
#model.adjR2s= NULL 
#
#for (i in 1:length(model)) {
#    model.cvR2s[i]  = cv.R2(model[[i]])
#    model.R2s[i]    = summary(model[[i]])$r.squared
#    model.adjR2s[i] = summary(model[[i]])$adj.r.squared
#}
#
#
#pdf("i-model_crossvalR2.pdf")
#plot(model.cvR2s,
#     xlab="Model number", ylab="R2",
#     ylim=c(0.93,0.99),
#     type='o', col="red", pch=19, lty=1,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#lines(model.R2s, type='o', col='blue', lty=2, pch=12,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#lines(model.adjR2s, type='o', lty=4,
#     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
#dev.off()
#
#print("R squared")
#print(model.R2s)
#print("Adj. R squared")
#print(model.adjR2s)
#print("Cross validated R2")
#print(model.cvR2s)
#

sink()
