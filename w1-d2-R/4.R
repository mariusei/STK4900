#
# Exercises STK4900
# part 4
# Day 2 - week 1
#

# One way ANOVA

# Sorption rate of organic chemical solvents

solvents=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/solvents.txt",header=T)
solvents$type = factor(solvents$type)
solvents$type		# produces levels 1, 2, 3

boxplot(rate~type, data=solvents)

# Interpretation: large variance in data set 2: Chloroalkanes, lowest mean in 
# data set 3: Esters. 

# Perform: One way ANOVA
aov.solvents = aov(rate~type, data=solvents)
aov.solvents
summary(aov.solvents)

# Gives F-value of 24.51 which gives null hypothesis probability Pr(>F) ~1e-7
#		=> significant difference in sorption rates