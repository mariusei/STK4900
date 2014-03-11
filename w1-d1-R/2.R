#
# Exercises STK4900
# part 2
# Day 1 - week 1
#

# Bone mineral density

controlgr=c(0.228, 0.207, 0.234, 0.220, 0.217, 0.228, 0.209, 0.221, 0.204, 0.220, 0.203, 0.219, 0.218, 0.245, 0.210) 
treat=c(0.250, 0.237, 0.217, 0.206, 0.247, 0.228, 0.245, 0.232, 0.267, 0.261, 0.221, 0.219, 0.232, 0.209, 0.255)

# mean and standard deviations

mean(controlgr)
sd(controlgr)

mean(treat)
sd(treat)


# Find confidence interval, t-statistic and P-value
t.test(treat, controlgr, var.equal=TRUE)

# check CI
0.0162+0.0117
0.0162-0.0117

# t-value corresponds to the P-value (denoted "p-value") which is
# the probability where the t-value gives the number of sigmas required 
# to find that P-value

