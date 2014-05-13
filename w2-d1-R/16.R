#
# Exercises STK4900
# part 16
# Day 1 - week 2
#

# Confidence interval and test for two proportions

n1	=969
y1	=310
p1	=y1/n1
se1	=sqrt( p1*(1-p1)/n1 )
n2	=968
y2	=303
p2	=y2/n2
se2	=sqrt(p2*(1-p2)/n2)
se	=sqrt(se1^2 + se2^2)
change=p1-p2
margin=1.96*se
lower	=change - margin
upper	=change + margin

cbind(change,margin,lower,upper)
	# The change in support for Høyre is found to be 
	# 0.69% with a margin of error on +-4.14%. 

# b) Test null hypothesis - no change in support for H
p	=(y1 + y2)/(n1+n2)
se0	=sqrt(p*(1-p)/n1 + p*(1-p)/n2)
	# test statistic: z
z	= (p1-p2)/se0
pval	= 2*(1 - pnorm(abs(z)))
cbind(z,pval)
	# z statistic is not large, and the null hypothesis is probable
	# this can relate to the CI computed earlier: the s.err. is large enough
	# to encompass the null hypothesis (p1-p2 = 0).

# c) Comparision of proportions 
hoyre	= matrix(c(y1,y2,n1-y1, n2-y2), nrow=2)
hoyre
prop.test(hoyre,correct=F)
	# the null hypothesis is rejected for large chi^2. 
	# Here, chi^2 = 0.1066 = z^2
	# with equal p-value	=> the null hypothesis is NOT rejected

# d) Including Arbeiderpartiet
yap1	=263
yap2	=238
pap1	=yap1/n1
pap2	=yap2/n2
seap1	=sqrt( pap1*(1-pap1)/n1 )
seap2	=sqrt( pap2*(1-pap2)/n2 )
seap	=sqrt( seap1^2 + seap2^2 )

	# change
dAp	= pap1-pap2
	# confidence interval?
ciAp	= 1.96*seap
cbind(dAp, ciAp, dAp + ciAp, dAp - ciAp)
	# gives dAp = 2.56 % w/ std.err of 3.90 % => H0 is w/in CI!!!
	# NO CHANGE IN SUPPORT?
ap	= matrix(c(yap1,yap2, n1-yap1, n2-yap2), nrow=2)
ap
hoyre
prop.test(ap, correct=F)
	# Chi^2 is 1.6481, which is smaller than two and yields a non-vanishing
	# H0 => the null hypothesis is NOT rejected
	#	=> no change in support