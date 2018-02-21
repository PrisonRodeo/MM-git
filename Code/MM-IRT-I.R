###################################################
# Modern Measurement (Spring 2018)
#
# Item Response Models
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(lme4)
library(plm)
library(gtools)
library(plyr)
library(texreg)
library(statmod)
library(psych)
library(ltm)

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#################################
# Simulation... True Rasch data:

N <- 1000
K <- 10
set.seed(7222009)
Rasch1Data <- sim.irt(nvar=K,n=N,low=-3,high=3,
                  a=1,c=0,d=NULL,mu=0,sd=1,
                  mod="logistic")

Rasch1<-rasch(Rasch1Data$items)
summary(Rasch1)

pdf("Notes and Slides/Sim1PLM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Rasch1,main=" ")
dev.off()

# Next, with a discrimination parameter different from 1.0:

set.seed(7222009)
RaschAltData <- sim.irt(nvar=K,n=N,low=-3,high=3,
                      a=2,c=0,d=NULL,mu=0,sd=1,
                      mod="logistic")

RaschAlt<-rasch(RaschAltData$items)
summary(RaschAlt)

pdf("Notes and Slides/SimAltPLM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(RaschAlt,main=" ")
dev.off()

# Next, with different discrimination parameters:

set.seed(7222009)
Discrims <- runif(K,0.5,3)
Rasch2Data <- sim.irt(nvar=K,n=N,low=-3,high=3,
                        a=Discrims,c=0,d=NULL,mu=0,sd=1,
                        mod="logistic")

Rasch2<-ltm(Rasch2Data$items~z1)
summary(Rasch2)

pdf("Notes and Slides/Sim2PLM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Rasch2,main=" ")
dev.off()

pdf("Notes and Slides/Sim2Discrims.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Discrims,Rasch2$coefficients[1:K,2],pch="",
     xlab="Discrimination Parameters",xlim=c(0.5,3),
     ylab="Estimates",ylim=c(0.5,3))
text(Discrims,Rasch2$coefficients[1:K,2],
     labels=colnames(Rasch2Data$items))
abline(a=0,b=1)
dev.off()

# 3PLM with different "guessing parameters" for each
# item:
#
# Note: Boost the N...

N <- 10000
set.seed(7222009)
GuessThresh <- round(rbeta(K,1,8),digits=1)
Rasch3Data <- sim.irt(nvar=K,n=N,low=-3,high=3,
                      a=1,c=GuessThresh,d=NULL,mu=0,sd=1,
                      mod="logistic")

Rasch3 <- tpm(Rasch3Data$items)
summary(Rasch3)

pdf("Notes and Slides/Sim3PLM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Rasch3,main=" ")
dev.off()

pdf("Notes and Slides/Sim3GandDs.pdf",8,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(rep(1,times=K),Rasch3$coefficients[1:K,3],pch="",
     xlab="Discrimination Parameter = 1.0",xlim=c(0.5,1.5),
     ylab="Estimates",ylim=c(0.5,1.5))
text(rep(1,times=K),Rasch3$coefficients[1:K,3],
     labels=colnames(Rasch3Data$items))
abline(h=1,lty=2,lwd=2)
plot(GuessThresh,summary(Rasch3)$coefficients[1:K,1],pch="",
     xlab="Guessing Parameters",xlim=c(-0.1,0.5),
     ylab="Estimates",ylim=c(-0.1,0.5))
text(GuessThresh,summary(Rasch3)$coefficients[1:K,1],
     labels=colnames(Rasch3Data$items))
abline(a=0,b=1)
dev.off()

##########################
# SCOTUS voting example:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/SCOTUS-IRT.csv")
SCOTUS <- read.csv(text = url) 
rm(url)

head(SCOTUS,10)
summary(SCOTUS)

# 1PLM:

OnePLM<-rasch(SCOTUS[c(2:10)])
summary(OnePLM)

coef(OnePLM, prob=TRUE, order=TRUE)

# Alternative model constraining alpha = 1.0:

IRTData <- SCOTUS[c(2:10)]

AltOnePLM<-rasch(IRTData, constraint=cbind(length(IRTData)+1,1))
summary(AltOnePLM)

# 2PLM:

TwoPLM<-ltm(IRTData ~ z1)
summary(TwoPLM)

# 2PLM Probabilities and testing:

coef(TwoPLM, prob=TRUE, order=TRUE)
anova(OnePLM, TwoPLM)

# 3PLM:

ThreePLM<-tpm(IRTData)
summary(ThreePLM)

anova(TwoPLM, ThreePLM)

# Plots:

pdf("Notes and Slides/1PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OnePLM,lty=seq(1:9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="1PLM ICCs")
dev.off()

pdf("Notes and Slides/2PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(TwoPLM,lty=seq(1:9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="2PLM ICCs")
dev.off()

pdf("Notes and Slides/3PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(ThreePLM,lty=seq(1:9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="3PLM ICCs")
dev.off()

