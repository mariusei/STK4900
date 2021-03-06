# COMP EXERCISE 1
# STK4900   -   Spring 2014
# Marius Berge Eide

# Load data:
bears=read.table("bears.dat", header=T) #, sep="\t",na.strings=".")

weight = bears$WEIGHT
length = bears$LENGTH
chest  = bears$CHEST

par(ps = 22, cex = 2, cex.main = 2)

# Boxplot: weight, length, chest:
pdf('a1-weight_length_chest.pdf')
par(mfrow=c(1,3))
boxplot(weight, ylab="Weight [pounds]",
        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
boxplot(length, ylab="Length [inches]",
        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
boxplot(chest, ylab="Chest circumference [inches]",
        cex=3, cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
dev.off()

# Scatterplot: length vs weight and chest vs weight
pdf('a2-weight_length.pdf')
plot(weight, length, xlab="Weight [lbs]", ylab="Length [in]",
     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()

pdf('a3-weight_chest.pdf')
plot(weight, chest, xlab="Weight [lbs]", ylab="Chest circumference [in]",
     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
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
plot(weightmod_1, 1,
     col="blue", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()
pdf('c2-weightmod_2-residuals.pdf')
plot(weightmod_2, 1,
     col="blue", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
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
                 log(HEADLEN)+SEX+MONTH+log(AGE), data=bearsOld,
                 x=T, y=T)
sink("g_output.txt")
print(summary(weightmod_5))
sink()


### H) CROSS-VALIDATED R SQUARED

cv.R2=function(lmobj,y=lmobj$y,x=lmobj$x)
{
   a=t(x)%*%x
   d=diag(1/eigen(a)$values)
   e=eigen(a)$vector
   inv.a=e %*% d %*% t(e)
   v=diag(x%*%inv.a%*%t(x))
   SSkryss=sum((lmobj$res/(1-v))^2)
   SStot=sum((y-mean(y))^2)
   R2kryss=1-SSkryss/SStot
   R2kryss
}

print("Cross-validated R2 for huge dataset:")
print(cv.R2(weightmod_5))


### I) FORWARD SELECTION

sumry = function(arg){
    lm(arg, data=bearsOld, x=T,y=T)
}

model = lapply(c(
        log(WEIGHT)~log(CHEST),
        log(WEIGHT)~log(CHEST)+log(LENGTH),
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH),
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE),
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH,
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
        log(HEADLEN),
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
        log(HEADLEN)+log(NECK),
        log(WEIGHT)~log(CHEST)+log(LENGTH)+log(HEADWTH)+log(AGE)+MONTH+
        log(HEADLEN)+log(NECK)+SEX
                 ), sumry)

model.cvR2s = NULL 
model.R2s   = NULL 
model.adjR2s= NULL 

for (i in 1:length(model)) {
    model.cvR2s[i]  = cv.R2(model[[i]])
    model.R2s[i]    = summary(model[[i]])$r.squared
    model.adjR2s[i] = summary(model[[i]])$adj.r.squared
}


pdf("i-model_crossvalR2.pdf")
plot(model.cvR2s,
     xlab="Model number", ylab="R2",
     ylim=c(0.93,0.99),
     type='o', col="red", pch=19, lty=1,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
lines(model.R2s, type='o', col='blue', lty=2, pch=12,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
lines(model.adjR2s, type='o', lty=4,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()

print("R squared")
print(model.R2s)
print("Adj. R squared")
print(model.adjR2s)
print("Cross validated R2")
print(model.cvR2s)

