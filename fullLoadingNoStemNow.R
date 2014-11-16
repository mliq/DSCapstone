# Start the clock!
ptm <- proc.time()

# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)

# FUNCTION DEFINITIONS #

# Make Corpus, Transform, Make Trigram TDM
makeTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<- TermDocumentMatrix(corpus)
#tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}

## DATA MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#

fileMunge<- function(x) {
text<-readLines(x)
totalLines=length(text)
chunkSize=20000
chunks=totalLines/chunkSize
remainder = chunks %% 1
wholeChunks = chunks-remainder
# initialize list
output=list()
# break file into chunks 
i=1
line=1
while (i<=wholeChunks){
end=line+chunkSize-1
output[[i]]<-text[line:end]
line=end+1
i=i+1
}
output[[i]]<-text[line:totalLines]
# Text Transformations to remove odd characters #
output=lapply(output,FUN=iconv, to='ASCII', sub=' ')
output=lapply(output,FUN= function(x) gsub("'{2}", " ",x))
output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
}

# Read, chunk, parse data, then make corpus, do transformations, make TDM of tri-grams:
twit<-fileMunge("en_US.twitter.txt")
tTDM <- makeTDM(twit)
# rm(twit)
gc()

news<-fileMunge("en_US.news.txt")
nTDM <- makeTDM(news)
# rm(news)
gc()

blog<-fileMunge("en_US.blogs.txt")
bTDM <- makeTDM(blog)
# rm(blog)
gc()

## Get unique words

tUgrams<-dimnames(tTDM)$Terms
bUgrams<-dimnames(bTDM)$Terms
nUgrams<-dimnames(nTDM)$Terms

### Number of Unique Words

tuwCount=length(tUgrams)
buwCount=length(bUgrams)
nuwCount=length(nUgrams)

## Get frequency table with word lengths.

charFreqTable<-function(x){
wLengths<-as.numeric((lapply(x,nchar)))
wlFreq=lapply(min(wLengths):max(wLengths),FUN=function(x){length(which(wLengths==x))})
counts=list(min(wLengths):max(wLengths))
return(data.frame(Chars=(min(wLengths):max(wLengths)),Freq=unlist(wlFreq)))
}

twlFreq<-charFreqTable(tUgrams)
bwlFreq<-charFreqTable(bUgrams)
nwlFreq<-charFreqTable(nUgrams)

# Plot

# Histogram for each corpus barplots:
par(mfcol=c(3,1))
## Line lengths in each Corpus, 3 histograms

## Word length Frequency Histograms for each Corpus
twLengths<-as.numeric((lapply(tUgrams,nchar)))
bwLengths<-as.numeric((lapply(bUgrams,nchar)))
nwLengths<-as.numeric((lapply(nUgrams,nchar)))

hist(twLengths, col ="cadetblue3", breaks=seq(0,max(twLengths),by=1), main="Twitter Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3, xlim=c(0,25))

hist(nwLengths, col ="chocolate2", breaks=seq(0,max(nwLengths),by=1), main="News Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3,xlim=c(0,25))

hist(bwLengths, col ="firebrick1", breaks=seq(0,max(bwLengths),by=1), main="Blogs Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3,xlim=c(0,25))

## AVERAGE word Length

saveRDS(twLengths,"twLengths")
saveRDS(nwLengths,"nwLengths")
saveRDS(bwLengths,"bwLengths")

twLengths=readRDS("twLengths")
bwLengths=readRDS("bwLengths")
nwLengths=readRDS("nwLengths")
# Stop the clock
proc.time() - ptm # 1255.23
