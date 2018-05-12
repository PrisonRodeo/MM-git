###################################################
# Comparative Courts Conference
###################################################
# Load packages (install as needed), set options:

library(RCurl)
library(XML)
library(tm)
library(stopwords)
library(qdap)
library(topicmodels)
library(stm)
library(wordcloud)
library(RColorBrewer)
library(colorRamps)

# setwd("~/Dropbox (Personal)/Ft. Worth")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

####################################################
# Grab SCONM 2016 decisions:

##  base urls:
url <- "http://www.nmcompcomm.us/nmcases/NMARYear.aspx?db=scr&y=2016"
url2 <- "http://www.nmcompcomm.us/nmcases/"

## query the url to get all the file names ending in '.pdf'

pdfs <- XML::getHTMLLinks(
  url, 
  xpQuery = "//a/@href['.pdf'=substring(., string-length(.) - 3)]"
)

dir.create("ZREFS") ## create a new directory 'ZREFS' to hold the downloads
wd <- getwd() ## save the current directory path
setwd("ZREFS") ## change working directory for the download
file.create(pdfs) ## create all the new files
lapply(paste0(url2, pdfs), function(x) download.file(x, basename(x))) ## download them all
setwd(wd) ## reset working directory

######################################
# Build a corpus, stem, etc.:

Dfiles <- list.files(path="ZREFS/",pattern="pdf") 

Dpdf<-readPDF(control = list(text = "-layout"))

DNM<-VCorpus(URISource(paste0("ZREFS/",Dfiles)), 
             readerControl = list(reader = Dpdf))

DNM.TDM <- TermDocumentMatrix(DNM, 
              control=list(language="en",tolower=TRUE,
              removePunctuation=TRUE,stopwords=TRUE,
              stemming=TRUE,removeNumbers=TRUE)) 
inspect(DNM.TDM) # Term-document matrix

# Topic models...

k <- 8 # N of topics
DNM.T <- LDA(t(DNM.TDM),k)

WORDZ <- get_terms(DNM.T,k)
WORDZ

gammaDF <- as.data.frame(DNM.T@gamma) 
rownames(gammaDF) <- DNM.TDM$dimnames$Docs
colnames(gammaDF) <- paste0(rep("Topic ",k),seq(1,k))

CBFriend<-c("#ca0020","#f4a582","#92c5de","#0571b0") # colors

# Heatmap of case classifications:

pdf("NM-heatmap.pdf",9,6)
heatmap(as.matrix(gammaDF),
        col=colorRampPalette(CBFriend)(40))
dev.off()