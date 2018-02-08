###################################################
# Modern Measurement (Spring 2018)
#
# PCA and FA...
#
###################################################
# Load packages (install as needed), set options:

library(RCurl)
library(psych) 
library(car)
# install.packages("randomNames")
library(randomNames)

# setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 12) # bias against scientific notation
options(digits = 4) # show fewer decimal places

###################################################
# PCA example:

X <- data.frame(X1=c(0,1,2),X2=c(6,5,3),X3=c(7,9,10))
X

CX <- sweep(M,2,colMeans(M),"-")  # "centered" M
CX

# Covariances and correlations:

Sigma <- cov(CX)
Sigma

R <- cor(CX)
R

# Eigenvalues and eigenvectors:

E <- eigen(Sigma)
E
L <- E$values
V <- E$vectors

sum(E$values)
tr(Sigma)

# PCScores <- Sigma*V
# PCScores

# SVD:

SVD <- svd(CX)
SVD
S <- SVD$d
U <- SVD$u
otherV <- SVD$v

# Eigenvalues:

(S^2)/(2)

# PCA:

princomp(CX) # via eigenvalues
prcomp(CX) # via SVD
otherV # from -svd-

###################################################
# PCA intuition figure:

set.seed(7222009)
Y1 <- rnorm(400)
Y2 <- Y1 + rnorm(400)
fit<-lm(Y2~Y1)

pdf("Notes and Slides/PCA-Intuition.pdf",10,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Y1,Y2,xlim=c(-4,4),ylim=c(-4,4),pch=20,
     xaxt="n",yaxt="n")
abline(h=0,lwd=2)
abline(v=0,lwd=2)
plot(Y1,Y2,pch=20,xlim=c(-4,4),ylim=c(-4,4),
     xaxt="n",yaxt="n",xlab="",ylab="")
abline(fit,lwd=2)
abline(a=fit$coefficients[1], b=-(1/fit$coefficients[2]),
       lwd=2)
abline(h=0,lwd=1,lty=2)
abline(v=0,lwd=1,lty=2)
text(3.5,2.5,"PC1")
text(-3.3,2.5,"PC2")
text(3,0.3,"cos(theta) = L1")
dev.off()


###################################################
# PCA simulation...
#
# A small dataset with N=20 and K=10 variables. Six
# of the variables (Z1-Z6) are related to the latent
# Z; the other four (X1-X4) are related to the 
# latent X:

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
Z <- rnorm(N)
Z1 <- Z + 0.2*rnorm(N)
Z2 <- Z + 0.5*rnorm(N)
Z3 <- Z + 1*rnorm(N)
Z4 <- Z + 1.5*rnorm(N)
Z5 <- Z + 2*rnorm(N)
Z6 <- Z + 3*rnorm(N)

X <- rnorm(N)
X1 <- X + rnorm(N)
X2 <- X + rnorm(N)
X3 <- X + rt(N,5)
X4 <- X + rt(N,5)

df <- data.frame(Z1,Z2,Z3,Z4,Z5,Z6,X1,X2,X3,X4)
rownames(df)<-Name 

head(df)
cor(df)

# Flavors of PCA:

PCE <- princomp(df)
PCE

PCS <- prcomp(df,retx=FALSE)
PCS

# Using -principal-

PCSim1 <- principal(df, nfactors=1,rotate="none")
PCSim1

PCSim1$scores

# Two principal components:

PCSim2 <- principal(df, nfactors=2,rotate="none")
PCSim2

PCSim2$scores

# Three principal components:

PCSim3 <- principal(df, nfactors=3,rotate="none")
PCSim3


##############################
# Biplots:

foo<-prcomp(df)
foo$rotation[,1:2]

pdf("Notes and Slides/SimulatedBiplot.pdf",7,6)
par(mar=c(4,4,4,2))
biplot(foo,main=" ",cex=0.8)
dev.off()

# FA using simulated data:

FASim1 <- factanal(df,factors=1,scores="regression",
                   rotation="none")
print(FASim1,cutoff=0)

# Plot of factor loadings vs. correlations with "Z"

FA1Ls <- FASim1$loadings[1:10,1]
ZRs<-cor(cbind(Z,df))[2:11,1]

pdf("Notes and Slides/FAvsRealCorrs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FA1Ls,ZRs,pch="",xlim=c(-0.6,1.2),
     ylim=c(-0.6,1.2),xlab="Factor Loadings",
     ylab="Correlations with Z")
text(FA1Ls,ZRs,labels=names(ZRs))
abline(a=0,b=1,lwd=2)
dev.off()

# Two-factor model:

FASim2 <- factanal(df,factors=2,scores="regression",
                   rotation="none")
print(FASim2,cutoff=0)

# Three-factor model:

FASim3 <- factanal(df,factors=3,scores="regression",
                   rotation="none")
print(FASim3,cutoff=0)


#########################################
# Remix: ANES 2016 feeling thermometers
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

describe(Therms,range=FALSE)

# Factor Analysis, 1/2/3 factors:

FTFA1 <- fa(Therms,nfactors=1,fm="ml",
                  rotate="none")
print(FTFA1)
# plot(FTFA1,labels=colnames(Therms))

FTFA2 <- fa(Therms,nfactors=2,fm="ml",
            rotate="none")
print(FTFA2)

pdf("Notes and Slides/FATherm2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FTFA2,labels=colnames(Therms),cex=0.6,
     xlim=c(-1.5,1.5),ylim=c(-0.1,0.6))
dev.off()



##########################
# Rotation sim example...

RoNone <- fa(df,nfactors=2,rotate="none",fm="ml")
RoVM <- fa(df,nfactors=2,rotate="varimax",fm="ml")
RoQM <- fa(df,nfactors=2,rotate="quartimax",fm="ml")
RoOb <- fa(df,nfactors=2,rotate="oblimin",fm="ml")

pdf("Notes and Slides/RotationSims.pdf",11,8.5)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
plot(RoNone,labels=colnames(df),title="None")
plot(RoVM,labels=colnames(df),title="Varimax")
plot(RoQM,labels=colnames(df),title="Quartimax")
plot(RoOb,labels=colnames(df),title="Oblimin")
dev.off()

# Rotation: FTs:

FTNone <- fa(Therms,nfactors=2,rotate="none",fm="ml")
FTVM <- fa(Therms,nfactors=2,rotate="varimax",fm="ml")
FTQM <- fa(Therms,nfactors=2,rotate="quartimax",fm="ml")
FTOb <- fa(Therms,nfactors=2,rotate="oblimin",fm="ml")

pdf("Notes and Slides/RotationFTs.pdf",11,8.5)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
plot(FTNone,labels=colnames(Therms),title="None")
plot(FTVM,labels=colnames(Therms),title="Varimax")
plot(FTQM,labels=colnames(Therms),title="Quartimax")
plot(FTOb,labels=colnames(Therms),title="Oblimin")
dev.off()


###########################
# Dimensionality
#
# Scree plot w/"parallel" plots, simulated data:

FAP <- fa.parallel(df)

pdf("Notes and Slides/SimulatedScreePlot.pdf",7,6)
FAP <- fa.parallel(df)
dev.off()

# Same, with FT data:

pdf("Notes and Slides/FTScreePlot.pdf",6,6)
FAPTherm <- fa.parallel(Therms)
dev.off()

########################################
# SCOTUS votes example (binary data):
#
# Data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/SCOTUS-IRT.csv")
SCOTUS <- read.csv(text = url)
rm(url)

SCOTUS <- na.omit(SCOTUS)
head(SCOTUS)

# Correlations:

SCOTUSR <- cor(SCOTUS[,2:10])
SCOTUSRs <- as.vector(SCOTUSR)
SCOTUST <- tetrachoric(SCOTUS[,2:10])
SCOTUSTs <- as.vector(SCOTUST$rho)

pdf("Notes and Slides/SCOTUSPearsonVsTet.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUSRs,SCOTUSTs,xlim=c(0,1),ylim=c(0,1),pch=19,
     xlab="Pearson Correlations",
     ylab="Tetrachoric Correlations")
abline(a=0,b=1,lwd=2)
dev.off()

# Scree / parallel plot, using tetrachoric correlation:

SCOTUSScree <- fa.parallel(SCOTUS[,2:10],cor="tet")

pdf("Notes and Slides/SCOTUSScreePlot.pdf",6,6)
fa.parallel(SCOTUS[,2:10],cor="tet")
dev.off()

# FA on tetrachoric voting correlations:

SCOTUSFA <- fa(SCOTUS[,2:10],nfactors=2,rotate="varimax",
               fm="ml",cor="tet")
SCOTUSFA
