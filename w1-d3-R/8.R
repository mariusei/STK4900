#
# Exercises STK4900
# part 8
# Day 3 - week 1
#

# Coffee sales


cafeteria 	= seq(14)
cafeteria
dispensers	= c(0,0,1,1,2,2,4,4,5,5,6,6,7,7)
length(dispensers)
sales		= c(508.1, 498.4, 568.2, 577.3, 651.7, 657.0, 755.3, 758.9, 787.6, 792.1, 841.4, 831.8, 854.7, 871.4)
length(sales)

# Straight line
cafe.fit = lm(sales~dispensers+cafeteria)
cafe.coeffs	= lm(sales~dispensers+cafeteria)
summary(cafe.fit)

# Second order polynomial
cafe.fit2 = lm(sales~dispensers^2+cafeteria^2)
summary(cafe.fit2)

# Better fit with linear model