#
# Exercises STK4900
# part 15
# Day 1 - week 2
#

# Confidence interval for proportions

# a) Votes for Høyre

n = 968
y = 303
p = y/n
se = sqrt(p*(1-p)/n)
margin = 1.96 * se
lower = p-margin
upper = p+margin
cbind(p,margin,lower,upper)
	# margin of error: 0.02921293 or 2.92 %


# b) 193: FrP and 41: Sp
# 	repeat calculations for FrP and Sp
#	-> "margin of error" for these parties compared to Høyre's?

y_frp = 193
y_sp	= 41

p_frp = y_frp/n
p_sp	= y_sp/n


# FrP
se_frp = sqrt(p_frp*(1-p_frp)/n)
cbind(p_frp, 1.96*se_frp, p_frp - 1.96*se_frp, p_frp + 1.96*se_frp)
	# margin of error: 0.02516938 or 2.52 %

# Sp
se_sp = sqrt(p_sp*(1-p_sp)/n)
cbind(p_sp, 1.96*se_sp, p_sp - 1.96*se_sp, p_sp + 1.96*se_sp)
	# margin of error: 0.01268746 or 1.27 %

