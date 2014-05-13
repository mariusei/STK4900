#
# Exercises STK4900
# part 10
# Day 3 - week 1
#

# multiple linear regression
# Heart and Estrogen/Progestin Study


# You may read the HERS data into R and extract the women without diabetes by the commands, 

hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/hers.txt",sep="\t",header=T,na.strings=".")
hers.no	=hers[hers$diabetes==0, ]
hers.yes	=hers[hers$diabetes==1, ]
 
# Glucose levels for women who exercise=1

#a
summary(hers.no$glucose[hers.no$exercise==0])
summary(hers.no$glucose[hers.no$exercise==1])

boxplot(hers.no$glucose~hers.no$exercise)

# =>
# 	Mean and median are slightly higher in women 
#	who do not exercise at least 3 times a week


#b
#	is thera a difference in glucose level? Confidence interval

t.test(glucose~exercise, var.equal=TRUE, data=hers.no)

# Gives t-value on 3.8685 and p-value of 0.000113, which makes the H0 hypothesis insign
# 95 percent confidence interval:
# 	0.8346242 2.5509539


#c
#	simple linear regression with glucose level as outcome and exercise as predictor
fit.c = lm(glucose~exercise, data=hers.no)
summary(fit.c)

# same p-value for exercise with corresponding negative t-value


#d
# Perform a simple linear regression
# with glucose level as outcome and exercise, age, and BMI as predictors:
fit.d=lm(glucose~exercise+age+BMI,data=hers.no)

summary(fit.d)

# BMI is significant, also is exercise. Age is less significant w/ P=0.0713, t=1.805