###################################################
# Modern Measurement (Spring 2018)
#
# Scaling text.
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(statmod)
library(MASS)
library(Hmisc)
library(arm)
# install.packages("ctv")
# library(ctv)  <-- as necessary
# install.views("NaturalLanguageProcessing")
library(tm)
library(stopwords)
library(SnowballC)
library(tokenizers)
#install.packages("readtext") # <- uncomment as necessary
library(readtext)
library(stringr)
library(rvest)
library(quanteda)
library(devtools)
#devtools::install_github("conjugateprior/austin")
#library(austin) # <- not used here, but useful

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

####################################################
# 2016 Presidential debates texts (read locally 
# because I'm lazy):

f1<-"Data/Debates/Debate2016-1.txt"
d1<-readChar(f1,file.info(f1)$size)
f2<-"Data/Debates/Debate2016-2.txt"
d2<-readChar(f2,file.info(f2)$size)
f3<-"Data/Debates/Debate2016-3.txt"
d3<-readChar(f3,file.info(f3)$size)
rm(f1,f2,f3)

# Clean things up / annotate:

m1 <- str_locate_all(d1, pattern = "CLINTON|TRUMP|HOLT")
m1 <- m1[[1]]
m1 <- m1[,1]

res1 <- vector(mode = "character", length = length(m1) - 1)
for (i in 1:(length(m1)-1)) {
  res1[i] <- substr(d1,m1[i],m1[i+1]-1)
}
clinton1 <- res1[sapply(res1,function(x) grepl("CLINTON",x))]
trump1 <- res1[sapply(res1,function(x) grepl("TRUMP",x))]
holt <- res1[sapply(res1,function(x) grepl("HOLT",x))]

# Add labels:

Clinton <- rep("Clinton: First Debate",times=length(clinton1))
Trump <- rep("Trump: First Debate",times=length(trump1))
Holt <- rep("Holt",times=length(holt))

# Corpuses, using quanteda:

CC1 <- corpus(clinton1,docvars=data.frame(Speaker=Clinton))
CT1 <- corpus(trump1,docvars=data.frame(Speaker=Trump))
CH <- corpus(holt,docvars=data.frame(Speaker=Holt))

D1C<-CC1+CT1+CH

# Same for second and third debates:

m2 <- str_locate_all(d2, pattern = "CLINTON|TRUMP|RADDATZ|COOPER")
m2 <- m2[[1]]
m2 <- m2[,1]

res2 <- vector(mode = "character", length = length(m2) - 1)
for (i in 1:(length(m2)-1)) {
  res2[i] <- substr(d2,m2[i],m2[i+1]-1)
}
clinton2 <- res2[sapply(res2,function(x) grepl("CLINTON",x))]
trump2 <- res2[sapply(res2,function(x) grepl("TRUMP",x))]
raddatz <- res2[sapply(res2,function(x) grepl("RADDATZ",x))]
cooper <- res2[sapply(res2,function(x) grepl("COOPER",x))]

# Add labels:

Clinton <- rep("Clinton: Second Debate",times=length(clinton2))
Trump <- rep("Trump: Second Debate",times=length(trump2))
Raddatz <- rep("Raddatz",times=length(raddatz))
Cooper <- rep("Cooper",times=length(cooper))

# Corpuses:

CC2 <- corpus(clinton2,docvars=data.frame(Speaker=Clinton))
CT2 <- corpus(trump2,docvars=data.frame(Speaker=Trump))
CR <- corpus(raddatz,docvars=data.frame(Speaker=Raddatz))
CA <- corpus(cooper,docvars=data.frame(Speaker=Cooper))

D2C<-CC2+CT2+CR+CA

# Third debate:

m3 <- str_locate_all(d3, pattern = "CLINTON|TRUMP|WALLACE")
m3 <- m3[[1]]
m3 <- m3[,1]

res3 <- vector(mode = "character", length = length(m3) - 1)
for (i in 1:(length(m3)-1)) {
  res3[i] <- substr(d3,m3[i],m3[i+1]-1)
}
clinton3 <- res3[sapply(res3,function(x) grepl("CLINTON",x))]
trump3 <- res3[sapply(res3,function(x) grepl("TRUMP",x))]
wallace <- res3[sapply(res3,function(x) grepl("WALLACE",x))]

# Add labels:

Clinton <- rep("Clinton: Third Debate",times=length(clinton3))
Trump <- rep("Trump: Third Debate",times=length(trump3))
Wallace <- rep("Wallace",times=length(wallace))

# Corpuses, using quanteda:

CC3 <- corpus(clinton3,docvars=data.frame(Speaker=Clinton))
CT3 <- corpus(trump3,docvars=data.frame(Speaker=Trump))
CW <- corpus(wallace,docvars=data.frame(Speaker=Wallace))

D3C<-CC3+CT3+CW

# All three:

D123C <- D1C+D2C+D3C
summary(D123C, 12)

#####################
# Summary plot:

Info <- summary(D123C,n=nrow(D123C$documents))

pdf("Notes and Slides/DebateResponseLength.pdf",6,5)
par(mar=c(12,4,2,2))
with(Info, boxplot(Tokens~Speaker,log="y",las=2,
                   ylab="Response Length"))
dev.off()


######################
# Create DFM:

DDFM <- dfm(D123C,remove=stopwords("english"),stem=TRUE,
            remove_punct=TRUE,groups="Speaker")
topfeatures(DDFM,28)

# Word clouds!

pdf("Notes and Slides/DebateDTWordCloud.pdf",5,5)
par(mar=c(1,1,1,1))
textplot_wordcloud(DDFM[c(2,5,9),],min_count=20,random_order=FALSE,
                   color=RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

pdf("Notes and Slides/DebateHCWordCloud.pdf",5,5)
par(mar=c(1,1,1,1))
textplot_wordcloud(DDFM[c(1,4,8),],min_count=20,random_order=FALSE,
                   color=RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

# Word frequency plot:

DFreqs <- textstat_frequency(DDFM,n=50)

pdf("Notes and Slides/DebatesTopWords.pdf",7,5)
par(mar=c(4,24,2,2))
with(DFreqs, dotchart2(frequency,labels=feature,pch=17,cex=0.5,
                       horizontal=TRUE,xlab="Frequency",
                       width.factor=2,dotsize=2))
dev.off()


# Word Keyness, first debate:

D1C2<-corpus_subset(D1C,Speaker %in% c("Clinton: First Debate",
                                        "Trump: First Debate"))
D1C2DFM <- dfm(D1C2,remove=stopwords("english"),stem=TRUE,
           remove_punct=TRUE,groups="Speaker")

D1Key <- textstat_keyness(D1C2DFM, target = "Clinton: First Debate")

head(D1Key,12)

pdf("Notes and Slides/D1WordKeynessPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_keyness(D1Key,margin=0.15,color=c("darkblue","red"),
                 labelsize=3)
dev.off()



#####################################
# Scaling: Wordfish (unsupervised):

WF <- textmodel_wordfish(DDFM,dir=c(1,2))
summary(WF)

WF.hats<-predict(WF,se.fit=TRUE)
WF.Bs <- coef(WF,margin="both")

# Rope and ladder plot of betas:

pdf("Notes and Slides/WordfishLadderPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF)
dev.off()

# Plot of word/feature scores:

pdf("Notes and Slides/WordfishFeaturesPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF,margin="features",alpha=0.4,
                 highlighted=names(topfeatures(DDFM,20)),
                 highlighted_color="red")
dev.off()


# Wordfish w/o moderators:

CTonly <- corpus_subset(D123C,Speaker=="Clinton: First Debate" |
                          Speaker=="Clinton: Second Debate" | 
                          Speaker=="Clinton: Third Debate" |
                          Speaker=="Trump: First Debate" |
                          Speaker=="Trump: Second Debate" |
                          Speaker=="Trump: Third Debate",
                          select=Speaker)
CTDFM <- dfm(CTonly,remove=stopwords("english"),stem=TRUE,
                     remove_punct=TRUE,groups="Speaker")

WF2 <- textmodel_wordfish(CTDFM,dir=c(1,2))
summary(WF2)

WF2.hats<-predict(WF2,se.fit=TRUE)

# Rope and ladder plot:

pdf("Notes and Slides/WordfishLadderPlot2.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2)
dev.off()

# Plot of word/feature scores:

pdf("Notes and Slides/WordfishFeaturesPlot2.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2,margin="features",alpha=0.4,
                 highlighted=names(topfeatures(CTDFM,20)),
                 highlighted_color="red")
dev.off()

# Another one, this time with large values of psi
# highlighted:

tfoo <- abs(WF2$psi)>3.5

pdf("Notes and Slides/WordfishFeaturesPlot3.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2,margin="features",alpha=0.4,
                 highlighted=WF2$features[tfoo==TRUE],
                 highlighted_color="red")
dev.off()

###################################
# Scaling: Wordscores
#
# Work with the "candidates only" corpuses. 
# Set the statements made by Clinton in the first
# debate equal to -1, and those made by Trump
# to -1:

TScores <- c(-1,1,NA)

WS.train <- textmodel_wordscores(D1C2DFM,TScores,
                                 scale="linear")
summary(WS.train)

# Plot wordscores:

pdf("Notes and Slides/Wordscore-scores.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WS.train,alpha=0.7,
                 highlighted=c("email"))
dev.off()

# Predict to the second and third debates:

D23Corpus <- corpus_subset(CTonly,Speaker=="Clinton: Second Debate" | 
                        Speaker=="Clinton: Third Debate" |
                        Speaker=="Trump: Second Debate" |
                        Speaker=="Trump: Third Debate",
                        select=Speaker)
D23DFM <- dfm(D23Corpus,remove=stopwords("english"),stem=TRUE,
              remove_punct=TRUE,groups="Speaker")

WS.test <- predict(WS.train,D23DFM,se.fit=TRUE,interval="confidence")
WS.test

# Plot them:

pdf("Notes and Slides/WordScoreLadder.pdf",6,5)
with(WS.test, coefplot(fit[c(1,2,4,5,8,9),1],se.fit[c(1,2,4,5,8,9)],
                       varnames=names(fit[c(1,2,4,5,8,9),1]),
                       main="",mar=c(2,10,2,2)))
dev.off()


# Rescaled wordscores:

WS.test2 <- predict(WS.train,D23DFM,se.fit=TRUE,interval="confidence",
                   rescaling="lbg")
WS.test2


# Code template for plots:
#
# pdf("Notes and Slides/ZZZZZZZZ.pdf",6,5)
# plot(g,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
#      main="GRM Item Response Category Characteristic Curves",
#      xlab=expression(paste(theta)))
# dev.off()