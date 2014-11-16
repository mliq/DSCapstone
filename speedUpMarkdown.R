
# Start the clock!
ptm <- proc.time()

# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
load("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US/RanMdownScriptFullProcessing.RData")

# Line counts

tlCount=2360148
nlCount=1010242
blCount=899288

## Number of Total Words

twCount=30359852
nwCount=34365936
bwCount=37334114

## Get maximum line length

tlmount=173
nlmount=11384
blmount=40833

## Get unique words

tUgrams<-dimnames(tTDM)$Terms
bUgrams<-dimnames(bTDM)$Terms
nUgrams<-dimnames(nTDM)$Terms

### Number of Unique Words

tuwCount=454293
buwCount=355364
nuwCount=79427

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

## AVERAGE unique word Length

tproduct=lapply(1:nrow(twlFreq),FUN=function(x){(twlFreq[x,1]*twlFreq[x,2])})
tAvg=(sum(unlist(tproduct))/sum(twlFreq$Freq)) # total chars

bproduct=lapply(1:nrow(bwlFreq),FUN=function(x){(bwlFreq[x,1]*bwlFreq[x,2])})
bAvg=(sum(unlist(bproduct))/sum(bwlFreq$Freq)) # total chars

nproduct=lapply(1:nrow(nwlFreq),FUN=function(x){(nwlFreq[x,1]*nwlFreq[x,2])})
nAvg=(sum(unlist(nproduct))/sum(nwlFreq$Freq)) # total chars

# Plot

# 3 corpora in one plot barplots:

## Shared parameters

names=c("Twitter Corpus", "News Corpus", "Blogs Corpus")
colors=c("cadetblue3","chocolate2","firebrick1")
par(mai=c(1,2,1,1))

## Number of Lines

counts=c(tlCount,blCount,nlCount)
bp<-barplot(counts, col=colors, main="Number of Lines", horiz=TRUE, names.arg=names, las=1, axes=FALSE)
axis(1, at = c(0,500000,1000000,1500000,2000000), labels=c("0","500,000","1,000,000","1,500,000","2,000,000"))
text(0,bp,prettyNum(counts,big.mark=",",scientific=F),cex=1.3,pos=4) 

## Average Unique Word Length

counts=c(tAvg,bAvg,nAvg)
bp<-barplot(counts, col=colors, main="Average Unique Word Length", horiz=TRUE, names.arg=names, las=1, axes=FALSE, xlim=c(0,10))
axis(1, at = c(0,2.5,5,7.5,10), labels=c(0,2.5,5,7.5,10))
text(0,bp,prettyNum(counts,big.mark=",",scientific=F),cex=1.3,pos=4) 

## Number of Unique Words
counts=c(tuwCount,buwCount,nuwCount)
bp<-barplot(counts, col=colors, main="Number of Unique Words", horiz=TRUE, names.arg=names, las=1, axes=FALSE, xlim=c(0,500000))
axis(1, at = c(0,100000,200000,300000,400000,500000), labels=c(0,"100,000","200,000","300,000","400,000","500,000"))
text(0,bp,prettyNum(counts,big.mark=",",scientific=F),cex=1.3,pos=4) 

# Histogram for each corpus barplots:
par(mfcol=c(3,1))
## Line lengths in each Corpus, 3 histograms

## Word length Frequency Histograms for each Corpus
twLengths<-as.numeric((lapply(tUgrams,nchar)))
bwLengths<-as.numeric((lapply(bUgrams,nchar)))
nwLengths<-as.numeric((lapply(nUgrams,nchar)))

hist(bwLengths, col ="firebrick1", breaks=seq(0,max(bwLengths),by=1), main="Blogs Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3,xlim=c(0,25))

hist(nwLengths, col ="chocolate2", breaks=seq(0,max(nwLengths),by=1), main="News Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3,xlim=c(0,25))

hist(twLengths, col ="cadetblue3", breaks=seq(0,max(twLengths),by=1), main="Twitter Corpus Word-Lengths", xlab = "Values", cex.lab = 1.3, xlim=c(0,25))

## AVERAGE word Length


# Stop the clock
proc.time() - ptm # 8.91 secs!
