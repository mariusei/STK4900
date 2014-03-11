#
# Exercises STK4900
# part 3
# Day 1 - week 1
#

# Historical measurements on the speed of light

speed=scan("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/exer2.dat")
speed


# a) Plot in various ways:
#	Histogram
#	Ecdf
#	Box plot
# -> Outliers?

hist(speed, main="Speed of light measurements", xlab="Speed [x10^23 + 24 millionth of a second]")
plot(ecdf(speed))
boxplot(speed)


# b) Empirical mean and median
mean(speed)
median(speed)

# Mean lower than median, indicates possible outliers?

# c) Empirical stddev and the interquartile range (diff betw third and first quart)
sd(speed)
quantile(speed)
IQR(speed)
plot(c(quantile(speed)[3]-quantile(speed)[1], quantile(speed)[4]-quantile(speed)[2], quantile(speed)[5]-quantile(speed)[3]))


# d) Compute a 95% confidence interval using ALL data
t.test(speed)

# w/o outliers
t.test(speed[speed>0])
