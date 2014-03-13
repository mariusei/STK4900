#
# Exercises STK4900
# part 5
# Day 2 - week 1
#

# Correlation and regression

#person = numeric(8)
person = seq(1,8)
pef	 = c(494, 395, 516, 434, 476, 413, 442, 433)
minipef= c(512, 430, 520, 428, 500, 364, 380, 445)

datafr = data.frame(person, pef, minipef)

yrange<- range(c(pef,minipef))
plot(person, pef, ylim=yrange)
points(minipef, col="red")

# Make categories
#person.types = factor(person)

plot(pef, minipef)

boxplot(pef,minipef)

# Calculate Pearson's r-coefficient
cov(pef,minipef)
cov(pef,minipef)/(sd(pef)*sd(minipef))

cor(pef,minipef)
cor(minipef,pef)

cor.test(pef,minipef)
# Gives 95 % confidence interval
# 0.2605298 0.9653948


# Fit: linear regression model with "minipef" as the outcome and "pef" as the
# predictor.
#	=> Estimated slope? (Least square estimate for pef)
?lm
linregrmod = lm(pef~minipef)
summary(linregrmod)
# Gives slope:
#	=> Estimate[2]: 0.5712
# Gives intercept:
#	=> Estimate[1]: 194.8143

x = seq(1,8)
y = 194.8143 + 0.5172 * pef
plot(pef,y)
plot(pef,minipef)
abline(pef,y)
