#
# Exercises STK4900
# part 4
# Day 1 - week 1
#

# Bootstrapping

# minerals from the Black Forest

rock.age=c(249, 254, 243, 268, 253, 269, 287, 241, 273, 306, 303, 280, 260, 256, 278, 344, 304, 283, 310)

bootsamp=sample(rock.age, replace=TRUE)
bootsamp
length(bootsamp)
length(rock.age)
sort(bootsamp) == sort(rock.age)


# Compute mean and median of the bootstrap sample
mean(bootsamp)
median(bootsamp)
sd(bootsamp)

mean(rock.age)
median(rock.age)
sd(rock.age)



# Draw 1000 samples w/ replacement
bootagemean<-numeric(0)

for (i in 1:1000) bootagemean[i]<-mean(sample(rock.age,replace=T))
sort(bootagemean)[c(025,975)]

t.test(bootagemean)