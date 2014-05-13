#
# Exercises STK4900
# part 19
# Day 2 - week 2
#

# A second look at logistic regression

# Data from Western Collaborative Group Study (WCGS)
wcgs=
read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v11/wcgs.txt",
sep="\t",header=T,na.strings=".")
#summary(wcgs)

expcoef=function(glmobj)
{ 
regtab=summary(glmobj)$coef 
expcoef=exp(regtab[,1]) 
lower=expcoef*exp(-1.96*regtab[,2]) 
upper=expcoef*exp(1.96*regtab[,2]) 
cbind(expcoef,lower,upper)
}

# a) CHD model using predictors:
#	age
#	chol
#	systol blod pres
#	smoking binary
#	behav pat	categorial

wcgs$behcat=factor(wcgs$behpat)
wcgs.beh=glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+behcat,
		 data=wcgs, family=binomial, subset=(chol<600)) 
summary(wcgs.beh) 

	# Behavioural patterns:
	# behcat2 increases risk of CHD, but null hypothesis is significant
	# behcat3 reduces risk of obt CHD, with null hyp rejected
	# behcat4 reduces risk of obt CHD, but null hypothesis is significant

# b) considering only TWO behavioural groups, A and B behav
	# dibpat: 	0 for B3 and B4
	#		1 for A1 and A2
wcgs.beh2=glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+dibpat, data=wcgs,
family=binomial, subset=(chol<600))
summary(wcgs.beh2)
	# A type behaviour has significant increasing effect on CHD
anova(wcgs.beh2,wcgs.beh, test="Chisq")
	# The Deviance is 0.23205 (chi squared distributed) which gives
	# the null hypothesis significance and it cannot be rejected.
	#	The models are the SAME

# c) model w/o the behavioural pattern and compare the three models
wcgs.resc=glm(chd69~age_10 + chol_50+bmi_10 + sbp_50+smoke,
data=wcgs, family=binomial, subset=(chol<600))

anova(wcgs.resc,wcgs.beh2,wcgs.beh, test="Chisq")
	# model 2 has large deviance, which REJECTS the null hypoth
	# model 3 has small deviance. The null hypoth is confirmed

# Preferred model: model 2: DIBPAT: two beh groups, A and B
# OR?

expcoef(wcgs.beh2)
	# Gives odds ratio of 2.01 : 1 of obtaining CHD if one belongs to
	# group A rather than group B.

#                expcoef      lower      upper
#(Intercept) 0.03359428 0.02512283 0.04492233
#age_10      1.83025048 1.44753341 2.31415509
#chol_50     1.70240514 1.46583042 1.97716134
#sbp_50      2.46791666 1.64802505 3.69570393
#bmi_10      1.73236105 1.02991824 2.91389615
#smoke       1.82916180 1.38725756 2.41183250
#dibpat      2.00685487 1.51225096 2.66322626
#


