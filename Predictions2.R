#Predictions:#
# Start the clock!
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(RWeka)

## FUNCTIONS ##

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
# corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
# corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

process<- function(x) {
# Text Transformations to remove odd characters #
# replace APOSTROPHES OF 2 OR MORE with space - WHY??? that never happens..
	# output=lapply(output,FUN= function(x) gsub("'{2}"rr, " ",x))
# Replace numbers with spaces... not sure why to do that yet either.
	# output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
# Erase commas.
x=gsub(",?", "", x)
# Erase ellipsis
x=gsub("\\.{3,}", "", x)
# Erase colon
x=gsub("\\:", "", x)
##### SENTENCE SPLITTING AND CLEANUP
# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
# ok here is where you change structure fundamentally... 
x<-strsplit(unlist(x),"[\\.]{1}")
x<-strsplit(unlist(x),"\\?+")
x<-strsplit(unlist(x),"\\!+")
x<-strsplit(unlist(x),"\\-+")
# Split also on parentheses
x<-strsplit(unlist(x),"\\(+")
x<-strsplit(unlist(x),"\\)+")
# split also on quotation marks
x<-strsplit(unlist(x),"\\\"")
# remove spaces at start and end of sentences:
# HERE is where the problem begins. why?
x<-gsub("^\\s+", "", unlist(x))
x<-gsub("\\s+$", "", unlist(x))
# Replace ~ and any whitespace around with just one space
x<-gsub("\\s*~\\s*", " ", unlist(x))
# Replace forward slash with space
x<-gsub("\\/", " ", unlist(x))
# Replace + signs with space
x<-gsub("\\+", " ", unlist(x))
# Eliminate empty and single letter values (more?)
x=x[which(nchar(x)!=1)]
x=x[which(nchar(x)!=0)]
}

## INPUT MUNGING ##

# Take an input:
test=readLines("Quiz2.txt")

# transform as training set was (lowercase, stem, strip punctuation etc.)
test=iconv(test, to='ASCII', sub=' ')
test=process(test)
corpus<-makeCorpus(test)
corpus<-unlist(lapply(corpus, FUN=function(x){as.character(x[1])}))

# Split by words:
words<-lapply(corpus,FUN=function(x){unlist(strsplit(x,"\\s+"))})

## PREDICTION TABLE LOOKUPS

# starting with trigram probabilities #

# First get the total number of trigrams
tTotal=Tfreq[,sum(counts)]

# now get the count of the top predicted trigram

# first I need the last two words of the sentence(s)
history=lapply(words,function(x){x[(length(x)-1):length(x)]})
history=lapply(history,function(x){paste(as.character(x),collapse=' ')})

# Load the twitter trigram data.table
library(data.table)
Tfreq=readRDS("t.Tfreq.RDS")

# Make prediction list of matches:
Tpred=lapply(history,function(x){Tfreq[grep(paste0("^",x),Tfreq$grams),][order(-counts)]})

# First Prediction Probability:

# isolate trigram back to only the predicted word.
pred=data.table(gram=Tpred[[1]][1]$grams,triProb=(Tpred[[1]][1]$counts/tTotal))

# bigram probability:
Bfreq=readRDS("t.Bfreq.RDS")
bHistory=lapply(words,function(x){x[(length(x)-1)]})
Bpred=lapply(bHistory,function(x){Bfreq[grep(paste0("^",x),Bfreq$grams),][order(-counts)]})

####DATA.TABLE REFERENCE####
# Subset listed predictions:
# Tpred[[1]][Tpred[[1]]$counts>5]
# how about search:
# test=Tfreq[counts>2]
# Fuzzy / Inexact Matches:
# test[grep("a lot",test$grams)]
# Only the first two words of trigram:
# test[grep("^mani time",test$grams)]
# grepl to get line numbers?
# example(data.table)
# Join two tables and match the keys, summing the values:
# DT=merge(DT1,DT2,all=TRUE)
# DT[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = names]
# DT[,c("counts.x","counts.y") := NULL]
####DATA.TABLE REFERENCE####