###################################################
# Modern Measurement (Spring 2018)
#
# Item Response Models (part deux: miscellaneous)
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(statmod)
library(MASS)
library(psych)
library(polycor)
# library(sem) # for SEMS... which we're skipping
# install.packages("ltm")
library(ltm)
# install.packages("mirt")
library(mirt)
# install.packages("flirt")
# library(flirt) # don't bother unless you have MATLAB
# install.packages("lavaan")
# library(lavaan) # for SEMs...
# install.packages("randomForest")
library(randomForest)
# install.packages("party")
library(party)

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#################################
# Draw a GRM:

set.seed(7222009)
dd <- sim.poly(nvar=5,n=1000,low=-2,high=2,
                     a=1,cat=4) # 5-category ordinal items
g <- grm(dd$items)

pdf("Notes and Slides/GRM-example.pdf",6,5)
plot(g,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
     main="GRM Item Response Category Characteristic Curves",
     xlab=expression(paste(theta)))
dev.off()

#################################
# Draw a PCM:

pc <- gpcm(dd$items)

pdf("Notes and Slides/PCM-example.pdf",6,5)
plot(pc,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
     main="PCM Item Response Category Characteristic Curves",
     xlab=expression(paste(theta)))
dev.off() 



#################################
# Simulation... ordinal data:

N<-1000
K<-9
set.seed(7222009)
Discs <- runif(K,0.5,2)
df <- sim.poly(nvar=K,n=N,low=-2,high=2,
               a=Discs,cat=5) # 5-category ordinal items
polychoric(df$items)

GRM <- grm(df$items,Hessian=TRUE)

pdf("Notes and Slides/GRM-ICCs.pdf",8,8)
par(mfrow=c(3,3))
par(mar=c(4,4,2,2))
plot(GRM)
dev.off()


Ufit <- mirt(df$items,1,itemtype="graded",se=TRUE)

Bifit <- mirt(df$items,2,se=TRUE)


#################################
# POLITY IV (2016) Data example:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/POLITY2016.csv")
POLITY <- read.csv(text = url) 
rm(url)

PIRT <- with(POLITY,
             data.frame(Country=scode,Year=year,
                        POLITYscore=polity2,
                        XRREG=xrreg,XRCOMP=xrcomp,
                        XROPEN=xropen,XCONST=xconst,
                        PARREG=parreg,PARCOMP=parcomp))

# Missing data, etc:

PIRT[PIRT == -88] <- NA
PIRT[PIRT == -77] <- NA
PIRT[PIRT == -66] <- NA
PIRT <- na.omit(PIRT)

describe(PIRT)
cor(PIRT[,3:9])

# Use IRT to create an alternative...

POLITY.GRM <- grm(PIRT[,4:9])
POLITY.GRM

title<-0.9
pdf("Notes and Slides/POLITY-GRM.pdf",9,6)
par(mfrow=c(2,3))
plot(POLITY.GRM,item=1,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.GRM,item=2,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.GRM,item=3,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.GRM,item=4,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.GRM,item=5,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.GRM,item=6,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
dev.off()

# Scores:

factor.scores(POLITY.GRM,robust.se=TRUE)

# Assign to country/years:

GRMscores <- factor.scores(POLITY.GRM,resp.patterns=PIRT[,4:9],
                           robust.se=TRUE)
PIRT$GRMs <- GRMscores$score.dat[,9]
PvGRM <- with(PIRT,cor(POLITYscore,GRMs)) # correlation...

pdf("Notes and Slides/POLITY-vs-GRMs.pdf",6,5)
par(mar=c(4,4,2,2))
with(PIRT, plot(POLITYscore,GRMs,pch=20,
                xlab="POLITY score",ylab="GRM score"))
text(-5,1,labels=paste("r =",round(PvGRM,2)))
dev.off()

# Over-time averages:

PAnnual <- ddply(PIRT, .(Year), summarize, 
                 PA=mean(POLITYscore),GRMA=mean(GRMs))

pdf("Notes and Slides/POLITY-GRMs-Annual.pdf",6,5)
par(mar = c(5, 4, 2, 5) + 0.3)
with(PAnnual, plot(Year,PA,t="l",lwd=2,ylab="POLITY scores")) # first plot
par(new = TRUE)
with(PAnnual, plot(Year,GRMA,t="l",axes=FALSE,bty="n",
                   xlab="",ylab="",lwd=2,lty=2,col="red"))
axis(side=4, at = pretty(range(PAnnual$GRMA)),col="red")
mtext("GRM scores", side=4, line=3,col="red")
dev.off()

# Same thing with GPCM:

set.seed(7222009)
POLITY.PCM <- gpcm(PIRT[,4:9],start.val="random",
                   control=list(iter.qN=1000))
POLITY.PCM

title<-0.9
pdf("Notes and Slides/POLITY-PCM.pdf",9,6)
par(mfrow=c(2,3))
plot(POLITY.PCM,item=1,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.PCM,item=2,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.PCM,item=3,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.PCM,item=4,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.PCM,item=5,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
plot(POLITY.PCM,item=6,lwd=2,lty=c(1,2,3,4,5,6),annot=TRUE,
     xlab=expression(paste(theta)),cex.main=title)
dev.off()

# Scores:

PCMscores <- factor.scores(POLITY.PCM,resp.patterns=PIRT[,4:9],
                           robust.se=TRUE)
PIRT$PCMs <- PCMscores$score.dat[,9]
PCMvGRM <- with(PIRT,cor(GRMs,PCMs)) # correlation...

pdf("Notes and Slides/GRMs-vs-PCMs.pdf",6,5)
par(mar=c(4,4,2,2))
with(PIRT, plot(GRMs,PCMs,pch=20,
                xlab="GRM score",ylab="PCM score"))
text(-1.8,-0.25,labels=paste("r =",round(PCMvGRM,2)))
dev.off()

#########################################
# Assess dimensionality (binary items):

N <- 1000
K <- 5
set.seed(7222009)
Discrims <- runif(K,1,3)
D1 <- sim.irt(nvar=K,n=N,low=-3,high=3,
              a=Discrims,c=0,d=NULL,mu=0,sd=1,
              mod="logistic")
Discrims <- runif(K,1,3)
D2 <- sim.irt(nvar=K,n=N,low=-3,high=3,
              a=Discrims,c=0,d=NULL,mu=0,sd=1,
              mod="logistic")
Data<-cbind(D1$items,D2$items)
colnames(Data)<-c("V1","V2","V3","V4","V5",
                  "X1","X2","X3","X4","X5")
tetrachoric(Data)

Model<-ltm(Data~z1)
summary(Model)

# Plot ICCs:

pdf("Notes and Slides/TwoDimICCs.pdf",6,5)
par(mar=c(4,4,4,2))
plot(Model,zrange=c(-7,7),lwd=2,
     lty=c(1,1,1,1,1,2,2,2,2,2))
dev.off()

# Dimensionality test:

UDTest<-unidimTest(Model)
UDTest

pdf("Notes and Slides/DimTestPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UDTest, type="b",pch=c(1,2),lty=c(1,1))
legend("topright", c("Real Data", "Average Simulated Data"),
       lty=1,pch=1:2,col=1:2,bty="n")
segments(2,2.1,2,2.9,lty=2,lwd=2,col="blue")
dev.off()

##########################
# Multidimensional IRT:

SIM.MIRT <- mirt(Data,2,itemtype="2PL")
summary(SIM.MIRT)

POLITY.MIRT <- mirt(PIRT[,4:9],2,itemtype="graded")
# POLITY.MIRT # check convergence diagnostics
summary(POLITY.MIRT)

# Plot response score surface:

pdf("Notes and Slides/MIRT-Polity-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(POLITY.MIRT, type="score",main=" ",
     rot=list(xaxis=-70,yaxis=30,zaxis=10))
dev.off()

####################################
# Random forest stuff...

source("Code/FunctionsRFclustering.txt")

no.forests<-20 # N of forests
no.trees<-500 # N of trees per forest

RF.sim <- RFdist(Data,mtry1=3,no.trees,no.forests,addcl1=T,
                addcl2=F,imp=T, oob.prox1=T)

# MDS on the results:

cmd1<-cmdscale(as.dist(RF.sim$cl1),2)

# Plotting:

pdf("Notes and Slides/RF-MDS-SimPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(cmd1,pch=20,xlab="First Dimension",ylab="Second Dimension")
dev.off()

# POLITY example:
#
# Speed up...

no.forests<-20 # N of forests
no.trees<-500 # N of trees per forest

RF.POLITY <- randomForest(x=PIRT[,4:9],ntree=no.trees,
                          importance=TRUE,proximity=TRUE)

# MDS and plotting:

POLITY.MDS <-cmdscale(as.dist(RF.POLITY$proximity),2)

pdf("Notes and Slides/RF-MDS-POLITYPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(POLITY.MDS,pch=20,xlab="First Dimension",
     ylab="Second Dimension")
dev.off()

# Variable importance:

MDA <- RF.POLITY$importance[,3]
MDG <- RF.POLITY$importance[,4]

pdf("Notes and Slides/RF-VIP-POLITYPlot.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
dotchart(MDA,labels=names(RF.POLITY$imp1),main=" ",pch=19,
         xlab="Mean Decrease in Accuracy")
dotchart(MDG,labels=names(RF.POLITY$imp1),main=" ",pch=19,
         xlab="Mean Decrease in Gini")
dev.off()
