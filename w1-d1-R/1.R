#
# Exercises STK4900
# part 1
# Day 1 - week 1
#

# minerals from the Black Forest

minerals.age=c(249, 254, 243, 268, 253, 269, 287, 241, 273, 306, 303, 280, 260, 256, 278, 344, 304, 283, 310)

# Mean, median and stddev
mean(minerals.age)
median(minerals.age)
sd(minerals.age)


# histogram
hist(minerals.age)

# Empirical distribution function
minerals.ecdf = ecdf(minerals.age)
plot(minerals.ecdf)
plot(minerals.ecdf, verticals=TRUE, do.points=FALSE)

# Minimum, first quartile, median, third quartile and max
quantile(minerals.age)

# Boxplot
boxplot(minerals.age,
		main="Black forest minerals", ylab="Age [Myr]")

####
# Confidence intervals and hypothesis testing
###

# 97.5% percentile of the Student's t-distribution w/ 18 degrees of freedom
# 	-> corresponding to "c" in slides!
#	-> corresponding to number of sigmas deviated from the mean!
qt(0.975,18)

# lower and upper limit of the 95% confidence interval
mean(minerals.age) - qt(0.975, 18) * sd(minerals.age) / sqrt(19)
mean(minerals.age) + qt(0.975, 18) * sd(minerals.age) / sqrt(19)


# Compute t-statistic
minerals.tstat = ( mean(minerals.age) - 265) / (sd(minerals.age) / sqrt(19) )
minerals.tstat

# P-value
1 - pt(minerals.tstat,18)

# Compute the confidence interval using R
t.test(minerals.age, mu=265)

# One-sided confidence interval
t.test(minerals.age, alternative="greater", mu=265)