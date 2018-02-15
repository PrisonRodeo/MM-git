###################################################
# Modern Measurement (Spring 2018)
#
# Cluster analysis.
#
###################################################
# Load packages (install as needed), set options:

library(RCurl)
library(psych) 
library(car)
library(cluster)
# install.packages("pvclust")
library(pvclust)
# install.packages("biotools")
library(biotools)
# install.packages("fpc")
library(fpc)
# install.packages("mclust")
library(mclust)
# install.packages("dendextend")
library(dendextend)
#install.packages("circlize")
library(circlize)
# install.packages("randomNames")
library(randomNames)

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 12) # bias against scientific notation
options(digits = 4) # show fewer decimal places

###################################################
# Silly Tick example:

Tick<-c(1,711,0.08)
Arthur<-c(0,588,0.27)
L2<-dist(rbind(Tick,Arthur))
L1<-dist(rbind(Tick,Arthur),method="manhattan")
# Mahalanobis by hand:
Diff<-Arthur-Tick
S <- cov(rbind(Tick,Arthur))
LM <- sqrt(t(Diff)%*%solve(S,tol=1e-30)%*%Diff)
# Check:
sqrt(mahalanobis(Arthur,center=Tick,cov=S,tol=1e-30))

#########################################
# Running Simulation Data:

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
X <- 5*rbeta(N,0.5,0.5)
Y <- runif(N,-4,4)
Z <- rbinom(N,1,pnorm(Y/2))

df <- data.frame(Name=Name,X=X,Y=Y,Z=Z)
rownames(df)<-df$Name

pdf("Notes and Slides/ClusterSimPlotOne.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("Notes and Slides/ClusterSimPlotTwo.pdf",3,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("Notes and Slides/ClusterSimPlotThree.pdf",6,3)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

# Distances:
#
# CENTER AND RESCALE / STANDARDIZE THE DATA:

ds <- scale(df[,2:4])

DL2 <- dist(ds) # L2 / Euclidean distance
DL1 <- dist(ds,method="manhattan") # L1 / Manhattan distance
DM <- sqrt(D2.dist(ds,cov(ds))) # Mahalanobis distances

# Scatterplot matrix of distances:

pdf("Notes and Slides/ClusterDistanceComparisons.pdf",6,5)
scatterplotMatrix(~DL2+DL1+DM,pch=20,
                  var.labels=c("Euclidean","Manhattan","Mahalanobis"))
dev.off()

#################################
# Agglomerative clustering (Euclidean distance):

ADL2.s <- hclust(DL2,method="single")
ADL2.c <- hclust(DL2,method="complete")
ADL2.a <- hclust(DL2,method="average")

str(ADL2.s)

pdf("Notes and Slides/ClusterSimDendrogram.pdf",4,6)
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single Linkage",xlab=" ",cex=0.8)
dev.off()

pdf("Notes and Slides/Cluster-HCLUST-Sims.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single",xlab=" ",cex=0.8)
plot(ADL2.c,main="Complete",xlab=" ",cex=0.8)
plot(ADL2.a,main="Average",xlab=" ",cex=0.8)
dev.off()

# Same thing, with Mahalanobis distance...

ADM.s <- hclust(DM,method="single")
ADM.c <- hclust(DM,method="complete")
ADM.a <- hclust(DM,method="average")

pdf("Notes and Slides/Cluster-HCLUST-Mahal.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADM.s,main="Single",xlab=" ",cex=0.8)
plot(ADM.c,main="Complete",xlab=" ",cex=0.8)
plot(ADM.a,main="Average",xlab=" ",cex=0.8)
dev.off()

#################################################
# Ward's Method:

ADL2.w <- hclust(DL2,method="ward.D2")

pdf("Notes and Slides/ClusterSimWardDendrogram.pdf",6,4)
par(mar=c(4,4,4,2))
plot(ADL2.w,main="Ward's Method",xlab=" ",cex=0.8)
dev.off()

#################################################
# Coefficient of Agglomeration

set.seed(7222009)
AC1 <- data.frame(A1=append(rnorm(10,10,1),rnorm(10,0.1)),
                  A2=append(rnorm(10,0,1),rnorm(10,10,1)))
AC2 <- data.frame(A1=runif(20,0,10),
                  A2=runif(20,0,10))

CAC1<-agnes(AC1,metric="euclidean",method="average")
CAC2<-agnes(AC2,metric="euclidean",method="average")

pdf("Notes and Slides/ClusterACExamplePlot.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(AC2,pch=20,xlab="X",ylab="Y",xlim=c(0,10),ylim=c(0,10))
legend("topleft",bty="n",legend=paste("AC = ",round(CAC2$ac,3)))
plot(AC1,pch=20,xlab="X",ylab="Y")
legend("topright",bty="n",legend=paste("AC = ",round(CAC1$ac,3)))
dev.off()

# Sim data:

Agnes.s <- agnes(ds, metric="euclidean",method="single")
Agnes.s$ac
Agnes.c <- agnes(ds, metric="euclidean",method="complete")
Agnes.c$ac
Agnes.a <- agnes(ds, metric="euclidean",method="average")
Agnes.a$ac
# Using Mahalanobis distance:
Agnes.M <- agnes(DM, diss=TRUE, method="average")
Agnes.M$ac

#################################################
# P-Values:

dst<-data.frame(t(ds))
PVDL2.s <- pvclust(dst,method.hclust="average",
                   method.dist="euclidean",nboot=1001)
PVDL2.s

pdf("Notes and Slides/ClusterPValueDendrogram.pdf",6,6)
par(mar=c(4,4,4,2))
plot(PVDL2.s,main="Euclidean/Single Linkage",xlab=" ",
     sub=" ",cex=0.8)
dev.off()

####################################
# Divisive clustering:

Diana.L2 <- diana(ds,metric="euclidean")
Diana.L2

Diana.L1 <- diana(ds,metric="manhattan")

pdf("Notes and Slides/DivisiveClusteringDendrograms.pdf",8,5)
par(mar=c(4,4,4,2))
par(mfrow=c(1,2))
plot(Diana.L2,which.plots=2,main="Euclidean Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L2$dc,3)))
plot(Diana.L1,which.plots=2,main="Manhattan Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L1$dc,3)))
dev.off()

###########################
# k-Means Clustering:

KM2 <- kmeans(ds,2)
KM2

# Plot:

pdf("Notes and Slides/KMeansPlot2.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM2$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 2",xlab="First PC",
         ylab="Second PC")
dev.off()

KM3 <- kmeans(ds,3)
KM3

pdf("Notes and Slides/KMeansPlot3.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM3$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 3",xlab="First PC",
         ylab="Second PC")
dev.off()

# Alternative:

PAM3 <- pam(ds,3)
PAM3

pdf("Notes and Slides/PAM3Cluster.pdf",6,5)
plot(PAM3,which.plots=1,main="PAM Cluster Plot (k=3)")
dev.off()


# Scree plot to determine number of clusters
wss <- (nrow(ds)-1)*sum(apply(ds,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(ds, 
                                     centers=i)$withinss)

pdf("Notes and Slides/ClusterKMeansScree.pdf",6,5)
par(mar=c(4,4,2,2))
plot(1:15, wss, t="o", xlab="Number of Clusters",pch=20,
     ylab="Within-Groups Sum of Squares",lwd=2)
dev.off()

# Model-based cluster choosing:

MCC <- Mclust(ds)
summary(MCC)

pdf("ModelBasedClusterPlot.pdf",6,5)
plot(MCC)
dev.off()


#################################################
# States 2005 data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/States2005.csv")
States <- read.csv(text = url)

summary(States)

StS <- data.frame(scale(States[,3:10]))
rownames(StS)<-States$statename

# Agglomerative Clustering:

StSL2 <- dist(StS) # L2 / Euclidean distance

StS.agg <- agnes(StS,metric="euclidean",method="average")

pdf("Notes and Slides/StatesAggDendrogram.pdf",8,6)
par(mar=c(4,4,4,2))
plot(StS.agg,which.plots=2,main="Euclidean Distance / Average Linkage",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",legend=paste("AC = ",round(StS.agg$ac,3)))
dev.off()

# Circular dendrogram using -dendextend-:

StS.dend <- hang.dendrogram(as.dendrogram(StS.agg))
StS.dend <- rotate(StS.dend, 1:50)
StS.dend <- color_branches(StS.dend, k=6)

pdf("Notes and Slides/StatesCircDendrogram.pdf",8,8)
par(mar=c(2,2,2,2))
StS.circ <- circlize_dendrogram(StS.dend,
             labels_track_height=0.4,
             dend_track_height=0.4)
dev.off()

# Kmeans: How many clusters?

wss <- (nrow(StS)-1)*sum(apply(StS,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(StS,centers=i)$withinss)

pdf("Notes and Slides/StatesKMeansScree.pdf",6,5)
par(mar=c(4,4,2,2))
plot(1:15, wss, t="o", xlab="Number of Clusters",pch=20,
     ylab="Within-Groups Sum of Squares",lwd=2)
dev.off()

# K-means plot, K=5:

StSKM5 <- kmeans(StS,5)

pdf("Notes and Slides/StateKMeans5.pdf",7,6)
par(mar=c(4,4,2,2))
clusplot(StS,StSKM5$cluster,color=TRUE,shade=TRUE,
         labels=3,lines=0,main=" ",xlab="First PC",
         ylab="Second PC",xlim=c(-7,4))
dev.off()

