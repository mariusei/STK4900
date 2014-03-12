# COMP EXERCISE 1
# STK4900   -   Spring 2014
# Marius Berge Eide

# Load data:
bears=read.table("bears.dat", header=T) #, sep="\t",na.strings=".")

weight = bears$WEIGHT
length = bears$LENGTH
chest  = bears$CHEST

# Boxplot: weight, length, chest:
pdf('a1-weight_length_chest.pdf')
par(mfrow=c(1,3))
boxplot(weight, ylab="Weight [pounds]")
boxplot(length, ylab="Length [inches]")
boxplot(chest, ylab="Chest circumference [inches]")
dev.off()

# Scatterplot: length vs weight and chest vs weight
pdf('a2-weight_length.pdf')
plot(weight, length, xlab="Weight [lbs]", ylab="Length [in]")
dev.off()

pdf('a3-weight_chest.pdf')
plot(weight, chest, xlab="Weight [lbs]", ylab="Chest circumference [in]")
dev.off()

# Are there any correlations?
print("Pearson cor. 1) Weight, length, 2) Weight, chest circum")
print(cor(weight, length))
print(cor(weight, chest))
print("Spearman cor. 1) Weight, length, 2) Weight, chest circum")
print(cor(weight, length, method="spearman"))
print(cor(weight, chest, method="spearman"))

### B) FIT LINEAR MODEL

weightmod_1 = lm(weight~length+chest)
weightmod_2 = lm(log(weight)~log(length)+log(chest))


### C) PLOTS OF RESIDUALS

pdf('c1-weightmod_1-residuals.pdf')
plot(weightmod_1, 1)
dev.off()
pdf('c2-weightmod_2-residuals.pdf')
plot(weightmod_2, 1)
dev.off()


### D) REMOVE OUTLIER no 52

bearsImp = bears[-52,]
weightmod_3 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST), data=bearsImp)
print(summary(weightmod_3))


### E) USE ONLY OLD BEARS
bearsOld = bearsImp[bearsImp$AGE>12,]
weightmod_4 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST), data=bearsOld)
print(summary(weightmod_4))


### F) FIND EXPECTED WEIGHT FOR 65 in long, 40 in round BEAR
print("Expected weight: ex f):")
print(exp(-6.061)*65^(.9934)*40^(1.983))


### G) USE ALL COVARIATES

bearsOld$SEX    = factor(bearsOld$SEX)
bearsOld$MONTH  = factor(bearsOld$MONTH)

weightmod_5 = lm(log(WEIGHT)~log(LENGTH)+log(CHEST)+log(NECK)+log(HEADWTH)+
                 log(HEADLEN)+SEX+MONTH+log(AGE), data=bearsOld)
sink("g_output.txt")
print(summary(weightmod_5))
sink()


