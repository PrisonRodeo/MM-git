###################################################
# Modern Measurement (Spring 2018)
#
# Scaling...
#
###################################################
# Load packages (install as needed), set options:

library(RCurl)
library(psych) # for /alpha
library(smacof) # MDS package
library(car) # car package for scatterplot matrices...

# setwd("~")   # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#######################
# UDS: Cities data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/USCities.csv")
Cities <- read.csv(text = url)
Cities <- Cities[1:10,]
rownames(Cities) <- Cities$city
rm(url)

# Create Euclidean distance matrix, 1D (longitude):

CityLong <- data.frame(t(Cities$longitude)) # longitudes in a row
colnames(CityLong) <- t(Cities$city) # names
D1long <- dist(t(CityLong)) # distance object

# UDS using uniscale in package(smacof):

UDS <- uniscale(D1long)
UDS
UDS$conf

# Plot:

pdf("UDS-Cities.pdf",6,4)
par(mar=c(4,4,4,4))
plot(UDS, 
     main="East-West Locations for the Ten Largest 
     U.S. Cities via UDS")
dev.off()

#######
# SCOTUS votes:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/SCOTUS-IRT.csv")
SCOTUS <- read.csv(text = url)
rm(url)

SCOTUS <- na.omit(SCOTUS)
head(SCOTUS)

# Sum Scores:

SumScores <- colSums(SCOTUS[,2:10],na.rm=TRUE) / nrow(SCOTUS)
SumScores

D1SCOTUS <- dist(t(SCOTUS[,2:10]))
D1SCOTUS

SCOTUS.UDS <- uniscale(D1SCOTUS)
SCOTUS.UDS

# Plot:

pdf("UDS-SCOTUS.pdf",6,6)
par(mar=c(2,2,4,2))
par(mfrow=c(2,1))
plot(SumScores,rep(0,times=9),type="p",xlab="",
     ylab="",axes=FALSE,main="Sum Scores",
     xlim=c(min(SumScores),max(SumScores)),pch=19)
text(SumScores,rep(0,times=9),labels=names(SumScores),
     srt=90,adj=c(-0.2,NA))
segments(0,0,1,0)
plot(SCOTUS.UDS,main="UDS Results")
dev.off()

##################################
# Kronbach's Alpha: simulation...
#
# Z is the underlying thing we're trying to measure...

N <- 400
set.seed(7222009)
Z <- rt(N,5)
plot(density(Z))

# Binary indicators... three strongly correlated with Z:

Z1 <- rbinom(N,1,pbinom(Z,1,0.5))
Z2 <- rbinom(N,1,pbinom(Z,1,0.5))
Z3 <- rbinom(N,1,pbinom(Z,1,0.5))

# ...two correlated with each other, uncorrelated with Z:

Y <- rt(N,5)
Y1 <- rbinom(N,1,pbinom(Y,1,0.5))
Y2 <- rbinom(N,1,pbinom(Y,1,0.5))

# ... and two uncorrelated with anything:

X1 <- rbinom(N,1,0.5)
X2 <- rbinom(N,1,0.5)

# Alpha:

simDF <- data.frame(cbind(Z1,Z2,Z3,Y1,Y2,X1,X2))
simAlpha <- alpha(simDF, check.keys=TRUE)
simAlpha

# Now with only the items that "should" be there:

simAlpha2 <- alpha(simDF[,1:3], check.keys=TRUE)
simAlpha2

# Alternative reliability measures:
#
# simRels <- glb(simDF)
# simRels

################
# Now a "real" data example: SCOTUS votes...

SCOTUSAlpha <- alpha(SCOTUS[,2:10],check.keys=TRUE)
SCOTUSAlpha



######################################
# MDS: ANES 2016 feeling thermometers
#
# Data stuff...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/ANES2016.csv")
ANES <- read.csv(text = url)
Tvars <- c("V162310","V162311","V162312","V162313",
           "V162314","V162078","V162079","V162080",
           "V162081","V162091","V162092","V162093",
           "V162094","V162095","V162096","V162097",
           "V162098","V162099","V162100","V162101",
           "V162102","V162103","V162104","V162105",
           "V162106","V162107","V162108","V162109",
           "V162110","V162111","V162112","V162113")

Therms <- ANES[Tvars]
Therms[Therms==-5] <- NA
Therms[Therms==-6] <- NA
Therms[Therms==-7] <- NA
Therms[Therms==-9] <- NA
Therms[Therms==998] <- NA
Therms[Therms==999] <- NA
Therms <- na.omit(Therms)
colnames(Therms) <- c("Asian-Americans","Hispanics","Blacks",
                      "Illegal Immigrants","Whites","Dem. Pres. Candidate",
                      "GOP Pres. Candidate","Libertarian Pres. Candidate",
                      "Green Pres. Candidate","Dem. VP", "GOP VP",
                      "John Roberts", "Pope Francis",
                      "Christian Fundamentalists","Feminists","Liberals",
                      "Labor Unions","Poor People","Big Business",
                      "Conservatives","SCOTUS","Gays & Lesbians",
                      "Congress","Rich People","Muslims","Christians",
                      "Jews","Tea Party","Police","Transgender People",
                      "Scientists","BLM")

# Summary:

pdf("ThermsBoxplot.pdf",6,5)
par(mar=c(11,4,1,2))
boxplot(Therms,notch=TRUE,las=2,pch=20,cex.axis=0.8)
dev.off()


# Distance thingy:

ThermDist <- dist(t(Therms))

################
# 2-D MDS...

MDS2.alt <- cmdscale(ThermDist,k=2)
head(MDS2.alt)

pdf("AltMDS2-Thermometers.pdf",6,5)
par(mar=c(4,4,2,2))
plot(MDS2.alt[,1], MDS2.alt[,2],type="n",xlab="First Dimension",
     ylab="Second Dimension",main=" ",xlim=c(-2500,2000))
text(MDS2.alt[,1],MDS2.alt[,2],labels(ThermDist),cex=0.9,
     xpd=TRUE)
dev.off()

# Alternative, using -mds-:

MDS2 <- mds(ThermDist, ndim=2)
MDS2

pdf("MDS2-Thermometers.pdf",6,5)
par(mar=c(4,4,2,2))
plot(MDS2, xlim=c(-1.5,1.5),cex=0.8,
     ylim=c(-1,1),main=" ")
dev.off()

# Compare the two:

pdf("MDS-Compare.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(MDS2.alt[,1],MDS2$conf[,1],pch=".",xlab="-cmdscale-",
     ylab="-mds-",main="First Dimension",xlim=c(-2500,2000))
text(MDS2.alt[,1],MDS2$conf[,1],cex=0.4,labels(ThermDist))
plot(MDS2.alt[,2],MDS2$conf[,2],pch=".",xlab="-cmdscale-",
     ylab="-mds-",main="Second Dimension",xlim=c(-1500,1500))
text(MDS2.alt[,2],MDS2$conf[,2],cex=0.4,labels(ThermDist))
dev.off()

########
# Variations on MDS...
#
# "Interval":

MDS2.int <- mds(ThermDist, ndim=2, type="interval")
MDS2.int

# Ordinal:

MDS2.ord <- mds(ThermDist, ndim=2, type="ordinal")
MDS2.ord

# Compare:

FirstDs<-data.frame(Ratio=MDS2.alt$conf[,1],
                    Interval=MDS2.int$conf[,1],
                    Ordinal=MDS2.ord$conf[,1])

SecondDs<-data.frame(Ratio=MDS2.alt$conf[,2],
                     Interval=MDS2.int$conf[,2],
                     Ordinal=MDS2.ord$conf[,2])

pdf("MDS-Therms-compareD1.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(FirstDs,pch=20,main="First Dimension")
dev.off()

pdf("MDS-Therms-compareD2.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SecondDs,pch=20,main="Second Dimension")
dev.off()


####################
# Diagnostics, etc.:
#
# Shepard diagrams:

pdf("MDS-Therm-Shepards.pdf",7,4)
par(mar=c(4,4,4,2))
par(mfrow=c(1,3))
plot(MDS2, plot.type="Shepard",main="Ratio")
plot(MDS2.int, plot.type="Shepard",main="Interval")
plot(MDS2.ord, plot.type="Shepard",main="Ordinal")
dev.off()

# Permutation test for goodness of fit:

ThermR.permtest<-permtest(MDS2,nrep=1000,verbose=FALSE)
ThermR.permtest
ThermI.permtest<-permtest(MDS2.int,nrep=1000,verbose=FALSE)
ThermI.permtest
ThermO.permtest<-permtest(MDS2.ord,nrep=1000,verbose=FALSE)
ThermO.permtest

# How does that look?

pdf("MDS-Therm-permtests.pdf",7,4)
par(mar=c(4,4,4,2))
par(mfrow=c(1,3))
plot(ThermR.permtest,main="Ratio")
plot(ThermI.permtest,main="Interval")
plot(ThermO.permtest,main="Ordinal")
dev.off()


# Same thing, using SCOTUS voting data...

SCR <- mds(D1SCOTUS, ndim=2, type="ratio")
SCR
SCI <- mds(D1SCOTUS, ndim=2, type="interval")
SCO <- mds(D1SCOTUS, ndim=2, type="ordinal")

# Example plot:

pdf("MDS2R-SCOTUS-plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCR, xlim=c(-1.5,1.5),cex=0.8,
     ylim=c(-1,1),main=" ")
dev.off()

SC1<-data.frame(Ratio=SCR$conf[,1],
                    Interval=SCI$conf[,1],
                    Ordinal=SCO$conf[,1])
SC2<-data.frame(Ratio=SCR$conf[,2],
                Interval=SCI$conf[,2],
                Ordinal=SCO$conf[,2])

JNames <- names(SCR$spp) # labels

pdf("MDS-SCOTUS-compareD1.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SC1,pch=20,main="First Dimension")
dev.off()

pdf("MDS-SCOTUS-compareD2.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SC2,pch=20,main="Second Dimension")
dev.off()

# Shepard plots:

pdf("MDS-SCOTUS-Shepards.pdf",7,4)
par(mar=c(4,4,4,2))
par(mfrow=c(1,3))
plot(SCR, plot.type="Shepard",main="Ratio")
plot(SCI, plot.type="Shepard",main="Interval")
plot(SCO, plot.type="Shepard",main="Ordinal")
dev.off()

# Permutation tests:
#
# Permutation test for goodness of fit:

SCOTUSR.permtest<-permtest(SCR,nrep=1000,verbose=FALSE)
SCOTUSR.permtest
SCOTUSI.permtest<-permtest(SCI,nrep=1000,verbose=FALSE)
SCOTUSI.permtest
SCOTUSO.permtest<-permtest(SCO,nrep=1000,verbose=FALSE)
SCOTUSO.permtest

# How does that look?

pdf("MDS-SCOTUS-permtests.pdf",7,4)
par(mar=c(4,4,4,2))
par(mfrow=c(1,3))
plot(SCOTUSR.permtest,main="Ratio")
plot(SCOTUSI.permtest,main="Interval")
plot(SCOTUSO.permtest,main="Ordinal")
dev.off()



########
# Cities, redux:

LL <- with(Cities, data.frame(Lat=latitude,
                              Lon=longitude))
rownames(LL)<-Cities$city
LLD <- dist((LL))
Cities2D <- mds(LLD,ndim=2)


plot(Cities2D,main="")

