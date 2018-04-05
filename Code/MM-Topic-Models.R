###################################################
# Modern Measurement (Spring 2018)
#
# Topic models.
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(lubridate)
library(stringr)
library(car)
library(statmod)
library(MASS)
# install.views("NaturalLanguageProcessing") # <-- as necessary
library(tm)
library(stopwords)
library(SnowballC)
library(topicmodels)
install.packages(wordcloud)
library(wordcloud)
install.packages("stm")
library(stm) 

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
####################################################


########################################
# UNHCR speech data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/UNHCRSpeeches.csv")
UN <- read.csv(text=temp,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

# Clean things up a bit:

UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Process text (this time using textProcessor from stm):
#
# Note that defaults convert cases, remove stopwords / 
# punctuation / words < 3 characters / extra white space, 
# and stems.

UNHCR <- textProcessor(UN$content, metadata=UN) 

# Create stm corpus. Note that this defaults to dropping
# words that only appear in one document:

UNCorp <- prepDocuments(UNHCR$documents,UNHCR$vocab,UNHCR$meta)

# Let's see what happens if we raise that lower threshold:

pdf("Notes and Slides/TopicDocRemoval.pdf",9,6)
plotRemoved(UNHCR$documents, lower.thresh = seq(1, 100, by = 5))
dev.off()

###################
# Basic LDA using topicmodels...
#
# Convert format:

UNLDACorp <- convertCorpus(UNCorp$documents,UNCorp$vocab,
                           type="slam")

# Basic LDA, with six topics:

UN.LDAV.6 <- LDA(UNLDACorp,6,method="VEM",
                seed=7222009) 

# Check out the terms / topics:

terms(UN.LDAV.6,10)

# ... and the topics:
#
# topics(UN.LDAV.6,10)

# Generate posterior probabilities of the topics 
# for each document and the terms for each topic:

V.6.Post <- posterior(UN.LDAV.6)
cor(V.6.Post$topics)

# Plot those:

pdf("Notes and Slides/LDA-Posteriors.pdf",9,7)
scatterplotMatrix(V.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                      colnames(V.6.Post$topics)))
dev.off()

# Examine topic probabilities by author:

pdf("Notes and Slides/LDA-By-Author.pdf",8,6)
par(mar=c(4,4,2,2))
par(mfrow=c(2,3))
boxplot(V.6.Post$topic[,1]~UN$Author,las=2,main="Topic One")
boxplot(V.6.Post$topic[,2]~UN$Author,las=2,main="Topic Two")
boxplot(V.6.Post$topic[,3]~UN$Author,las=2,main="Topic Three")
boxplot(V.6.Post$topic[,4]~UN$Author,las=2,main="Topic Four")
boxplot(V.6.Post$topic[,5]~UN$Author,las=2,main="Topic Five")
boxplot(V.6.Post$topic[,6]~UN$Author,las=2,main="Topic Six")
dev.off()

# Same model, using Gibbs sampling:
# 
# UN.LDAG.6 <- LDA(UNLDACorp,6,method="Gibbs",
#                  seed=7222009) 
# 
# # Examine topics:
# 
# terms(UN.LDAG.6,10)
#
# Loop over different numbers of topics, and
# check the perplexity for each:

MaxTopics <- 40
Seq <- seq(2,MaxTopics,by=2)
Perps <- numeric(MaxTopics/2)
for (i in Seq) {
  foo <- LDA(UNLDACorp,i,method="VEM",
             seed=7222009)
  Perps[i/2] <- perplexity(foo)
}

# Plot:

pdf("Notes and Slides/PerplexityByK.pdf",7,5)
par(mar=c(4,4,2,2))
plot(Seq,Perps,t="l",lwd=2,ylab="Perplexity",
     xlab="Number of Topics",xlim=c(0,41))
dev.off()

##################################
# Correlated topic models
#
# Basic CTM:

UN.CTMV.6 <- CTM(UNLDACorp,6,method="VEM",
                 seed=7222009) 

# Check out topics:

terms(UN.CTMV.6,10)

# Posteriors:

CTMV.6.Post <- posterior(UN.CTMV.6)
cor(CTMV.6.Post$topics)

pdf("Notes and Slides/CTM-Posteriors.pdf",9,7)
scatterplotMatrix(CTMV.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                             colnames(CTMV.6.Post$topics)))
dev.off()

##################################
# Structural topic model...
#
# One run:

STM.6 <- stm(UNCorp$documents,UNCorp$vocab,6,
             prevalence=~Year+Author,
             data=UNCorp$meta)

# Things you can do with a single STM...
#
# Check out the topics...

labelTopics(STM.6)


# Plots!
#
# Summary:

pdf("Notes and Slides/STM-Summary-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6, type="summary",labeltype="frex",
     main="Top Topics (FREX words)")
dev.off()

# MAP histograms:

pdf("Notes and Slides/STM-Hist-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6,type="hist",labeltype="frex")
dev.off()

# Labels:

pdf("Notes and Slides/STM-Labels-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6,type="perspectives",topics=c(5,1),
     labeltype="frex",plabels=c("Yugoslavia","Africa"))
dev.off()

# OMG WORD CLOUDS:

pdf("Notes and Slides/STM-Cloud-Plot.pdf",8,7)
par(mar=c(4,4,2,2))
cloud(STM.6,topic=5)
dev.off()

# Examine covariate effects:

UN$Ogata <- ifelse(UN$Author=="Ogata",1,0)
STM.Ogata<- estimateEffect(1:6~Ogata,STM.6,metadata=UN)
summary(STM.Ogata)

pdf("Notes and Slides/STM-Ogata-Effects.pdf",6,5)
par(mar=c(4,10,2,2))
plot(STM.Ogata,"Ogata",method="difference",
     cov.value1=1,cov.value2=0,xlab="Difference in Probability")
dev.off()

# Alternative model: Topical *content*:

STM.6C <- stm(UNCorp$documents,UNCorp$vocab,6,
             content=~Author,
             data=UNCorp$meta)
STMC.Ogata<- estimateEffect(1:6~Ogata,STM.6C,metadata=UN)


# Topic correlations:

STM.TopicR <- topicCorr(STM.6,method="huge")

pdf("Notes and Slides/STM-Topic-Corrs.pdf",6,6)
plot(STM.TopicR)
dev.off()


# Model selection:

STM.6.select <- selectModel(UNCorp$documents,UNCorp$vocab,6,
                prevalence=~Year+Author,data=UNCorp$meta,
                max.em.its=150,runs=20,seed=7222009)

multiSTM(STM.6.select)

# Selection of the number of topics:

STM.searchK <- searchK(UNCorp$documents,UNCorp$vocab,
        c(2,5,seq(10,50,by=10),75,100))

pdf("Notes and Slides/STM-Picking-K.pdf",7,5)
par(mar=c(4,4,4,2))
plot(STM.searchK,pch=20)
dev.off()




##################################
# Code template for plots:
#
# pdf("Notes and Slides/ZZZZZZZZ.pdf",6,5)
# plot(g,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
#      main="GRM Item Response Category Characteristic Curves",
#      xlab=expression(paste(theta)))
# dev.off()