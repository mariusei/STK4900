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

weightmod = lm(weight~length+chest)

