###################################################
# Modern Measurement (Spring 2018)
#
# Introduction to text...
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(statmod)
library(MASS)
# install.packages("ctv")
# library(ctv)  <-- as necessary
# install.views("NaturalLanguageProcessing")
library(tm)
library(stopwords)
library(SnowballC)
library(tokenizers)

setwd("~/Dropbox (Personal)/Modern Measurement")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
####################################################
# TF-IDF Toy:

A <- "red blue red"
B <- "green blue orange"
C <- "yellow blue yellow"

ToyC <- Corpus(VectorSource(cbind(A,B,C)))
as.matrix(TermDocumentMatrix(ToyC))
as.matrix(weightTfIdf(TermDocumentMatrix(ToyC)))

####################################################
# Basic text manipulation: a really tiny example:
#
# Raw text:

Walter <- "You want a toe? I can get you a toe, believe me. There are ways, Dude. You don't wanna know about it, believe me."

# Basic operations:
#
# Replace capitals (all-caps is "toupper"):

tolower(Walter)

# Replace characters (ex: "a" with "A"):

chartr("a","A",Walter)

# Punctuation removal:

removePunctuation(Walter)

# Remove words:

removeWords(Walter, "toe")

# From a list:

wordsGone<-c("toe","Dude","believe")
removeWords(Walter, wordsGone)

# Can also removeNumbers and stripWhitespace...

# Tokenize: Break into sentences:

Walter.sent <- tokenize_sentences(Walter)
Walter.sent
length(Walter.sent[[1]])

# Tokenize II: Break into words:

Walter.words <- tokenize_words(Walter)
Walter.words
length(Walter.words[[1]]) # total word count

# Tokenize III: Break sentences into words:

Walter.sw <- tokenize_words(Walter.sent[[1]])
Walter.sw

# Count words per sentence:

Walter.wordcount <- sapply(Walter.sw, length)
Walter.wordcount

# Term frequencies:

termFreq(Walter,control=list(removePunctuation=TRUE))

# N-grams: Basic N-grams of length 2:

Walter.Ng2<-tokenize_ngrams(Walter,n=2)
Walter.Ng2

# Count of unique N-grams of length 2:

table(Walter.Ng2)

# Skip N-grams: length=4, skip=1:

tokenize_skip_ngrams(Walter,n=4,k=1)

# Eliminate stop-words:

stopwords("en")
removeWords(Walter,stopwords("en"))

# Basic stemming (uses the Snowball stemmer):

stemDocument(Walter)

# Create a basic document (NLP package):

WS <- PlainTextDocument(Walter, author="Walter Sobchak",
                        description="Get you a toe",
                        language="en",
                        origin="The Big Lebowski")

str(WS)

# Creating a (simple) corpus from sentences/words (NLP package):

Walter.clean <- removePunctuation(Walter.sent[[1]])
WSC<-Corpus(VectorSource(Walter.clean))
inspect(WSC)
str(WSC)

# Term-Document Matrix:

WS.TDM <- TermDocumentMatrix(WSC,control=list(tolower=TRUE,
                                          stemming=TRUE))
inspect(WS.TDM)

# Document-Term Matrix:

WS.DTM <- DocumentTermMatrix(WSC,control=list(tolower=TRUE,
                                              stemming=TRUE))
inspect(WS.DTM)
as.matrix(WS.DTM)

# TF-IDF weighting:

WS.TFIDF <- weightTfIdf(WS.TDM)
as.matrix(WS.TFIDF)
as.matrix(WS.TDM)


# # Finding frequent words:
#
# findFreqTerms(WS.TDM,2,Inf)
# findFreqTerms(WS.TDM,3,Inf)
#
# # Term scores (counts of specific words) by document:
#
# tm_term_score(WS.TDM,"toe")
# tm_term_score(WS.TDM,"you")

# Associations:

cor(as.matrix(WS.DTM))

findAssocs(WS.TDM,"toe",0.3)

#
# For more text-y Dudeness, go here:
# http://www.awesomefilm.com/script/biglebowski.txt
#

#########################################
# Now we'll do something larger...the 2016 
# general election debates. Note that I'm
# just reading the files locally... you'll
# need to download them and fix the paths
# to make the code work.

Dfiles <- list.files(path="Data/Debates/",
                    pattern="pdf") 
Dpdf<-readPDF(control = list(text = "-layout"))

D16<-VCorpus(URISource(paste0("Data/Debates/",Dfiles)), 
            readerControl = list(reader = Dpdf))

# Now clean that mess up while creating the TDM:

D16.TDM <- TermDocumentMatrix(D16, 
            control=list(language="en",tolower=TRUE,
            removePunctuation=TRUE,stopwords=TRUE,
            stemming=FALSE,removeNumbers=FALSE)) 
inspect(D16.TDM)

# Associations:

findAssocs(D16.TDM,"clinton",0.98)
findAssocs(D16.TDM,"trump",0.98)

Rs<-findAssocs(D16.TDM,"clinton",0)

pdf("Notes and Slides/ClintonCorrs.pdf",6,5)
par(mar=c(4,4,2,2))
hist(Rs$clinton,main=" ",xlab="Correlation with 'Clinton'")
dev.off()

# Plot a TDM (required Rgraphviz):

source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)

D16.20MFT <- findMostFreqTerms(D16.TDM,20) # 20 most frequent terms
D16L <- names(D16.20MFT$`Debate2016-3.pdf`) # ...from debate #3

pdf("Notes and Slides/D16-TDM-plot.pdf",6,5)
plot(D16.TDM,terms=D16L,corThreshold=0.75)
legend("topright",bty="n",legend="Minimum r = 0.75")
dev.off()

# Weighting:

D16.TFW <- weightTf(D16.TDM)
D16.TFIDFW <- weightTfIdf(D16.TDM)

as.matrix(D16.TFW)[1:8,]
as.matrix(D16.TFIDFW)[1:8,]

# More:

TFs<-findMostFreqTerms(D16.TFW,n=20) # top-20 terms
TFIDFs<-findMostFreqTerms(D16.TFIDFW,n=20) # in each
cbind(names(TFs$`Debate2016-1.pdf`),c(names(TFIDFs$`Debate2016-1.pdf`)))

# TF-IDF-weighted plot:

D16.20MFTb <- findMostFreqTerms(D16.TFIDFW,20) # 20 most frequent terms
D16Lb <- names(D16.20MFTb$`Debate2016-3.pdf`) # ...from debate #3

pdf("Notes and Slides/D16-TFIDF-plot.pdf",6,5)
plot(D16.TFIDFW,terms=D16Lb,corThreshold=0.999)
legend("topright",bty="n",legend="TF-IDF Weights,\nMinimum r = 0.999")
dev.off()


# Code template for plots:
#
# pdf("Notes and Slides/ZZZZZZZZ.pdf",6,5)
# plot(g,items=3,lwd=2,lty=c(1,2,3,4),annot=TRUE,
#      main="GRM Item Response Category Characteristic Curves",
#      xlab=expression(paste(theta)))
# dev.off()