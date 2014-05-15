# PROJECT EXAM 
# STK4900   -   Spring 2014

### PROBLEM 1 ###
source("utils_STK4900.R")

# Load data:
uffi<-read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v14/uffi.txt",h=T)

CH2O    = uffi$CH2O
AIR     = uffi$AIR
UFFI    = factor(uffi$UFFI)

# Relation between CH20 and AIR tightness
par(ps = 22, cex = 2, cex.main = 2)
pdf('1a1-CH2O_air.pdf')
plot(AIR, CH2O, ylab='CH2O concentration [ppm]', xlab='Air tightness [0,10]'
     ,col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()

# Relation between CH20 concentration and presence of UFFI
pdf('1a2-CH2O_UFFI.pdf')
plot(UFFI, CH2O, ylab='CH2O concentration [ppm]', xlab='UFFI present'
     ,col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()

### 1b) Multiple regression
model = lapply(c(
        CH2O~UFFI,
        CH2O~AIR,
        CH2O~AIR+UFFI,
        CH2O~AIR+UFFI+AIR:UFFI,
        CH2O~AIR+UFFI+AIR:UFFI+I(AIR^2)
                 ), sumry)

model.cvR2s = NULL 
model.R2s   = NULL 
model.adjR2s= NULL 

for (i in 1:length(model)) {
    model.cvR2s[i]  = cv.R2(model[[i]])
    model.R2s[i]    = summary(model[[i]])$r.squared
    model.adjR2s[i] = summary(model[[i]])$adj.r.squared
}
for (i in 1:length(model)) {
    print(i)
    print(model.adjR2s[i])
}

pdf("1a3-model_crossvalR2.pdf")
plot(model.cvR2s,
     xlab="Model number", ylab="R2",
     ylim=c(0.0,1),
     type='o', col="red", pch=19, lty=1,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
lines(model.R2s, type='o', col='blue', lty=2, pch=12,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
lines(model.adjR2s, type='o', lty=1,
     cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()

# Checking model 3, as this is the one where R2_CV is highest
pdf('1c1-model3_residuals.pdf')
plot(lm(CH2O~AIR+UFFI),1, 
     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()
pdf('1c2-model3_stdresiduals.pdf')
plot(lm(CH2O~AIR+UFFI),3, 
     col="red", pch=19, cex=3, cex.main=2,cex.lab=1.8,cex.axis=1.8)
dev.off()
