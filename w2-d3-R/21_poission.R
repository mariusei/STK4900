#
# Exercises STK4900
# part 21
# Day 3 - week 2
#

# Fitting Poisson models
# The following function may be used to calculate the means,
# variances, coefficients of dispersion, 
# Pearson X2 with corresponding P-value from the three datasets. 
# Copy the commands below and paste them into R:

 
poissontab=function(counts,x=(0:(length(counts)-1)))
{
  n=sum(counts)
  meanx=sum(x*counts)/n
  varx=sum((x-meanx)^2*counts)/(n-1)
  CD=varx/meanx
  df=length(counts)-2
  px=c(dpois(0:df,meanx),1-ppois(df,meanx))
  EX=n*px
  tab=cbind(x,counts,round(EX,2),round(counts-EX,2),round((counts-EX)^2/EX,2))
  X2=sum((counts-EX)^2/EX)
  pval=1-pchisq(X2,df)
  print(paste("Mean=",round(meanx,4)))
  print(paste("Var= ",round(varx,4)))
  print(paste("CD= ",round(CD,4))) 
  tab=as.data.frame(tab)
  names(tab)=c("x","Obs","Exp","O-E","(O-E)^2/E")
  print(tab)
  print(paste("Pearson X2 =",round(X2,2)))
  print(paste("p-value    =",round(pval,4)))
}

 

# In order to apply the function to
# the first data set in the exercise, you give the commands:
yeast=c(75,103,121,54,30,13,4)
moss =c(100,9,6,8,1,0,2)
azukiwevils=c(61,50,1)
poissontab(yeast)

# Coefficient of dispersion
#	For Poisson processes, the variance and mean are the same.
#	The CD is the ratio between those. If sign higher than 1 => overdisp.

	# Yeast sample: CD approx 1. 
	# Chi squared test: all Poisson distributed:	H0
	# ???Chi squared goes to zero if H0 is rejected??? Depending on df
	# Chi squared = 7.47	H0 accepted
	# as p-value is 0.1877 (not insignificant)


# MOSS
poissontab(moss)
	# CD is 2.7		> overdispersed, the variance is larger than the mean
	# X2 is 2741.51	> H0 rejected
	# as p-value is 0

# AZUKI WEVILS
poissontab(azukiwevils)
	# CD is 0.5793	> underdispersed the variance is smaller 
	# X2 is 17.45 and H0 REJECTED as p-value is 0
 
