###################################################
# Modern Measurement (Spring 2018)
#
# Dictionary methods and sentiment analysis.
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(lubridate)
library(stringr)
library(car)
# install.packages("ctv")
# library(ctv)  <-- as necessary
# install.views("NaturalLanguageProcessing")
library(tm)
library(stopwords)
library(qdap)
library(SentimentAnalysis)

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
####################################################
# General Inquirer example:

data(DictionaryGI)

# AFINN example:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/AFINN-111.txt")
AFINN <- read.delim(text=temp,header=FALSE,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

AFINN<-AFINN[order(AFINN$V2),]
View(AFINN)

########################################
# UNHCR speech data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/MM-git/master/Data/UNHCRSpeeches.csv")
UN <- read.csv(text=temp,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

UN$content <- removeNumbers(UN$content) # no numbers
UN$content <- str_replace_all(UN$content, "[\n]", " ") # line breaks
UN$content <- removeWords(UN$content,stopwords("en")) # remove stopwords
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

# Corpus:

UN2 <- with(UN, data.frame(doc_id = id,
                           text = content))
ds <- DataframeSource(UN2) 
UNC <- Corpus(ds)
meta(UNC)

# Some tools in SentimentAnalysis...

UNCount<-countWords(UNC,removeStopwords=FALSE)
summary(UNCount$WordCount)

pdf("Notes and Slides/UNHCR-Hist.pdf",6,5)
par(mar=c(4,4,2,2))
hist(UNCount$WordCount,main=" ",xlab="Word Count",
     col="grey32")
dev.off()

######################################
# Simple sentiment analysis:

UNSent <- analyzeSentiment(UNC)
summary(UNSent)


# Plots, etc.:

rSC<-with(UNSent, cor(log(WordCount),SentimentGI))

pdf("Notes and Slides/UNHCRSentVsCount.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~WordCount,data=UNSent,log="x",
            pch=20,grid=FALSE,xlab="ln(Word Count)",
            ylab="Sentiment",spread=FALSE)
abline(h=0,lty=2,lwd=1.5)
text(100,0.25,paste0("r = ",round(rSC,2)))
dev.off()

pdf("Notes and Slides/UNHCRSentOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNSent$SentimentGI,t="l",lwd=1.5,
     xlab="Date",ylab="Speech Sentiment")
lines(lowess(UN$Date,UNSent$SentimentGI),lwd=2,col="red")
abline(h=0,lty=2)
dev.off()

# By year...

AnnMeans<-aggregate(UNSent$SentimentGI,list(UN$Year),mean)

pdf("Notes and Slides/UNHCRAnnMeans.pdf",6,5)
par(mar=c(4,4,2,2))
plot(AnnMeans$Group.1,AnnMeans$x,t="l",lwd=1.5,
     xlab="Year",ylab="Average Sentiment",ylim=c(0.04,0.17))
lines(lowess(AnnMeans$Group.1,AnnMeans$x),lwd=2,col="red")
dev.off()

# By author:

UN$Author<-ordered(UN$Author,levels=c("Goedhart","Lindt",
                             "Schnyder","Khan","Hartling",
                             "Hocké","Stoltenberg","Ogata",
                             "Lubbers","Guterres"))

pdf("Notes and Slides/UNHCR-by-Author.pdf",6,5)
par(mar=c(6,4,2,2))
boxplot(UNSent$SentimentGI~UN$Author,las=2)
abline(h=0,lty=2)
dev.off()


# Similar results by dictionary?

GI<-loadDictionaryGI()
QD<-loadDictionaryQDAP()

compareDictionaries(GI,QD)

r.GI.QDAP <- with(UNSent, cor(SentimentGI,SentimentQDAP))

pdf("Notes and Slides/UNHCR-Dict-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~SentimentQDAP,data=UNSent,
            xlab="QDAP",ylab="General Inquirer",pch=20,
            grid=FALSE)
text(0,0.20,paste0("r = ",round(r.GI.QDAP,2)))
dev.off()

# For which speakers does the dictionary matter?

DictDiff <- with(UNSent, abs(SentimentGI - SentimentQDAP))

summary(lm(DictDiff~UN$Author - 1))

# Custom dictionary by-hand:

YugoWords <- c("yugoslavia","serbia","bosnia","herzegovina",
               "kosovo","montenegro","macedonia","croatia",
               "vojvodina","balkans")

FmrYugo <- SentimentDictionaryWordlist(YugoWords)

UNHCRYugo <- analyzeSentiment(UNC,
                  rules=list("YugoTopic"=list(
                    ruleRatio,FmrYugo)))

summary(UNHCRYugo$YugoTopic)

pdf("Notes and Slides/UNHCRYugoOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNHCRYugo$YugoTopic,t="l",lwd=1.5,
     xlab="Date",ylab="Fmr. Yugoslavia Content")
dev.off()

# Using a weighted dictionary:

YugoWords
YugoScores <- c(1,3,3,3,3,2,2,2,2,1)

FmrYugo2 <- SentimentDictionaryWeighted(YugoWords,YugoScores)

UNHCRYugo2 <- analyzeSentiment(UNC,
                     rules=list("YugoTopic"=list(
                     ruleLinearModel,FmrYugo2)))
summary(UNHCRYugo2)

pdf("Notes and Slides/UNHCRWeightedYugoOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNHCRYugo2$YugoTopic,t="l",lwd=1.5,
     xlab="Date",ylab="Weighted Fmr. Yugoslavia Content")
dev.off()

###################
# Automatically generate a dictionary of words that
# are "decisive" (distinguishing) with respect to
# some response variable.
#
# Here, see what words distinguish Ogata (1991-2000)
# from the others:

UN$Ogata <- ifelse(UN$Author=="Ogata",1,0)

OgataDict <- generateDictionary(UNC,UN$Ogata,
                    modelType="lasso",
                    control=list(family="binomial"))

summary(OgataDict)
OgataDict
OgataD <- SentimentDictionaryBinary(OgataDict$words[OgataDict$scores>0],
                                    OgataDict$words[OgataDict$scores<0])

# now, "sentiment":

OgataSent <- analyzeSentiment(UNC,
                   rules=list("OgataScore"=list(
                   ruleSentiment,OgataD)))
summary(OgataSent)


pdf("Notes and Slides/OgataTimeSeries.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,OgataSent$OgataScore,t="l")
abline(h=0,lty=2)
dev.off()

# Assess performance:
#  - generate in-sample predictions:

OgataHat <- predict(OgataDict,UNC)
summary(OgataHat$Dictionary)

#  - plot them...

pdf("Notes and Slides/OgataPredictions.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(OgataHat$Dictionary),main=" ",
     ylab="Density",lwd=2,xlab="Predicted Document Scores")
dev.off()

#  - compare to the in-sample response:

compareToResponse(OgataHat,UN$Ogata)

#  - plot

pdf("Notes and Slides/OgataPredVsActual.pdf",6,5)
par(mar=c(4,4,2,2))
plotSentimentResponse(OgataHat,UN$Ogata)
dev.off()


##################################
# Code template for plots:
#
# pdf("Notes and Slides/ZZZZZZZZ.pdf",6,5)
# plot(g,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
#      main="GRM Item Response Category Characteristic Curves",
#      xlab=expression(paste(theta)))
# dev.off()