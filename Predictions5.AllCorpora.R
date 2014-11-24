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

#### INPUT MUNGING ####

# Take an input:
test=scan("Quiz2.txt", what="character",n=1,skip=0)

# transform as training set was (lowercase, stem, strip punctuation etc.)
test=iconv(test, to='ASCII', sub=' ')
test=process(test)
corpus<-makeCorpus(test)
corpus=as.character(corpus[[1]][1])

# Split by words:
words<-unlist(strsplit(corpus,"\\s+"))

# Isolate last two words of the sentence
history=words[(length(words)-1):length(words)]
nMin1=words[length(words)]
history=paste(as.character(history),collapse=' ')

#### TRIGRAM PREDICTION TABLES ####

# Load the twitter trigram data.table
library(data.table)
Tfreq=readRDS("t.Tfreq.RDS")

# Make prediction list of matches:
Tpred=data.table(Tfreq[grep(paste0("^",history),Tfreq$grams),][order(-counts)])

#### TRIGRAM Prediction Probabilities: ####

# First get the total number of trigrams
tTotal=Tfreq[,sum(counts)]

# Edit trigrams to just the prediction and the count
Tpred=Tpred[,{s=strsplit(grams," ");list(prediction=unlist(s)[c(FALSE,FALSE,TRUE)],counts=counts)}]

# Calculate probabilities:
Tpred[,probability:=counts/tTotal]

#### BIGRAM PREDICTION TABLES #####

# Load Twitter Bigram Table
Bfreq=readRDS("t.Bfreq.RDS")

# Make prediction list of matches
Bpred=data.table(Bfreq[grep(paste0("^",nMin1),Bfreq$grams),][order(-counts)])

#### BIGRAM Prediction Probabilities: ####

bTotal=Bfreq[,sum(counts)]

# Edit bigrams to just the prediction and the count
Bpred=Bpred[,{s=strsplit(grams," ");list(prediction=unlist(s)[c(FALSE,TRUE)],counts=counts)}]

# Calculate probabilities:
Bpred[,probability:=counts/bTotal]

#### MERGE BI AND TRI PREDICTION TABLES ####
setkey(Bpred,prediction)
setkey(Tpred,prediction)

TBpred=merge(Tpred,Bpred,all=TRUE,suffixes=c(".T",".B"))
TBpred[,probability:=(10^(sum(log(probability.T),log(probability.B),na.rm=TRUE))), by = prediction]
TBpred[,c("probability.T","probability.B") := NULL]
TBpred=TBpred[order(-probability)]

#### UNIGRAM PREDICTION TABLES ####

# Load Twitter Unigram Table
Ufreq=readRDS("t.Ufreq.RDS")

# No predictions in unigram case

#### UNIGRAM Prediction Probabilities: ####

uTotal=Ufreq[,sum(counts)]

# Calculate probabilities:
Ufreq[,probability:=counts/uTotal]

#### MERGE UNI WITH BI+TRI PREDICTION TABLE ####
setnames(Ufreq,"grams","prediction")
setkey(Ufreq,prediction)
setkey(TBpred,prediction)

UBTpred=merge(TBpred,Ufreq,all=TRUE,suffixes=c(".TB",".U"))
UBTpred[,probability:=(10^(sum(log(probability.TB),log(probability.U),na.rm=TRUE))), by = prediction]
UBTpred[,c("probability.TB","probability.U") := NULL]
UBTpred=UBTpred[order(-probability)]

# ok so now all is dandy except my predictions blow.
# study model again.
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
# help(":=")
# rename a column quickly:
# setnames(Ufreq,"old","new")
####DATA.TABLE REFERENCE####