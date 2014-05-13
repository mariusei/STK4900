#
# Exercises STK4900
# part 9
# Day 3 - week 1
#

# Insurance


phys 		= seq(18)
income	= c(47.35, 29.26, 52.14, 32.15, 40.86, 19.19, 27.23, 25.60, 54.14, 26.72, 38.84, 32.99, 32.95, 21.69, 27.90, 56.70, 37.69, 39.94)
length(income)
risk		= c(7,5,10,6,4,5,4,6,9,5,2,7,4,3,5,1,8,6)
lifeins	= c(140,45,180,160,90,10,35,35,190,35,75,70,55,10,40,175,95,95)
length(lifeins)


# Linear model: risk and income
data.incrisk = lm(risk~income)
summary(data.incrisk)
cor(risk,income)
cor(risk,lifeins)

# inear model: life insurance amount and risk
data.insrisk = lm(lifeins~risk)
summary(data.insrisk)
# gives t value on 1.740 amd P-value on .101
# 	t value = estimate / std error
# => insignificant

# linear model: life insurance amount and income
data.insinc = lm(lifeins~income)
summary(data.insinc)
# gives t value on -4.176 amd P-value on .000714
# 	t value = estimate / std error
# => SIGNIFICANT

# Overall

data.ins = lm(lifeins~risk+income)
summary(data.ins)


# Try higher order models?
data.insinc_log = lm(log(lifeins)~risk+log(income))
summary(data.insinc_log)

# Gives the best model?

data.insinc_2 = lm(lifeins~risk^2+income^2)
summary(data.insinc_2)

# was worse



# Plot a little bit
par(mfrow=c(1,2))
plot(income, lifeins)
plot(risk, lifeins)
par(mfrow=c(1,1))