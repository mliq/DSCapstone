# OK now I am going to run to get the data.tables for each, save.
# If this continues to go too fast, something is wrong with fileMunge... I'm cutting off most of the list perhaps?

# Start the clock!
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(RWeka)
options("max.print"=1000000)

# FUNCTION DEFINITIONS #

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
# corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
# corpus<- tm_map(corpus,removePunctuation)
# corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

# My original model worked by having a nested list where each chunk contained lines, so 2 levels. Make sure process returns the same.

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
x<-gsub("^\\s+", "", x)
x<-gsub("\\s+$", "", x)
# Replace ~ and any whitespace around with just one space
x<-gsub("\\s*~\\s*", " ", x)
# Replace forward slash with space
x<-gsub("\\/", " ", x)
# Replace + signs with space
x<-gsub("\\+", " ", x)
# Eliminate empty and single letter values (more?)
x=x[which(nchar(x)!=1)]
x=x[which(nchar(x)!=0)]
}

# Tokenizer functions
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
UgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Input File processing

fileMunge<- function(x) {
text<-readLines(x) #, warn=FALSE,encoding="UTF-8",skipNul=TRUE)
text=iconv(text, to='ASCII', sub=' ')
}

#=============================================#
#=============================================#

# DATA MUNGING #

# 1. Corpus, transformations, and TDM Creation
#=============================================#

# Read, chunk, parse data, then make corpus, do transformations, make TDM of tri-grams:
text=fileMunge("en_US.twitter.txt")
text=process(text)

corpus<-makeCorpus(text)

Ttdm<- TermDocumentMatrix(corpus, control = list(tokenize = TgramTokenizer))
gc()
Btdm<- TermDocumentMatrix(corpus, control = list(tokenize = BgramTokenizer))
gc()
Utdm<- TermDocumentMatrix(corpus, control = list(tokenize = UgramTokenizer))
gc()

rm(corpus)

#########################
#BUILD PREDICTION TABLES#
#########################

library(slam)
library(data.table)

counts=row_sums(Utdm)
rm(Utdm)
gc()
Ufreq<-data.table(grams=names(counts), counts=counts)
setkey(Ufreq,grams)

counts=row_sums(Btdm)
rm(Btdm)
gc()
Bfreq<-data.table(grams=names(counts), counts=counts)
setkey(Bfreq,grams)

counts=row_sums(Ttdm)
rm(Ttdm)
gc()
Tfreq<-data.table(grams=names(counts), counts=counts)
setkey(Tfreq,grams)
gc()

# Stop the clock
proc.time() - ptm # 10000 news lines: 74 secs 1.23 minutes

# so should be 109 minutes no oops i'm doing twitter again, so
# 2360148; 236*1.23=291/60= 5 hours! shit.
# guess I should maybe do that sampling thang...
# 100,000 lines would take? 12 minutes.
### CREATE TRAINING SET ###

set.seed(42)
t.train=sample(1:2360148,100000)
text<-readLines("en_US.twitter.txt")
text=iconv(text, to='ASCII', sub=' ')
train=text[t.train]
sink('t.train')
train
sink()
saveRDS(train, file="t.train.RDS")

# therefore, the full text of news corpus: 889288 = 100 minutes but potentially too much memory... 
#but wait, how much does R use up naturally? I need to get individual object sizes:
object.size(Bfreq) #18.5 MB
object.size(Tfreq) #33.3 MB
object.size(Ufreq) #1.9 MB
# That's great, 18 times above is still small, less than 1 GB.
# problem though is the corpus.
# Vcorpus is now 237.3 MB: times 18 = 4271.4
# Ok it can be done though.

# Cool, so data.table is actually smaller than TDMs. How can I remove sparse terms though, must I do this first in data.
# Can be done with counts. also in data.table: Ufreq[counts>8]
# how about search:
#test=Tfreq[counts>2]
#Fuzzy / Inexact Matches:
#test[grep("a lot",test$grams)]
# Only the first two words of trigram:
# test[grep("^mani time",test$grams)]
# grepl to get line numbers?
#=============================================#
# PREDICTION #
#=============================================#

## Prediction Corpus Preparation ##

# isolate the query words from the predicted final word of each n-gram:
length(as.character(Pfreq$grams[1]))

test<-unlist(strsplit(as.character(Pfreq$grams[1]),"\\s+"))

Tfreq$words<-lapply(Tfreq$grams,FUN=function(x){unlist(strsplit(as.character(x),"\\s+"))})
Tfreq$query<-lapply(Tfreq$words,FUN=function(x){paste(x[1:2],collapse=' ')})
Tfreq$result<-lapply(Tfreq$words,FUN=function(x){x[3]})

Bfreq$words<-lapply(Bfreq$grams,FUN=function(x){unlist(strsplit(as.character(x),"\\s+"))})
Bfreq$query<-lapply(Bfreq$words,FUN=function(x){x[1]})
Bfreq$result<-lapply(Bfreq$words,FUN=function(x){x[2]})

# now let's see if this is too slow... not really.


## INPUT MUNGING ##

# Take an input:
input<-readLines("Quiz2.txt")

# Perform Transformations:
input<-makeCorpus(input)
input<-lapply(input, FUN=function(x){as.character(x[1])})

# Split by words:
input<-lapply(input,FUN=function(x){unlist(strsplit(x,"\\s+"))})

## TESTING ##

for (j in 1:length(input)){
# First, by pentagrams
# Reduce 1st test sentence to last 4 words
for(i in 3:0) {
num<-(length(input[[j]])-i)
string<-input[[j]][num]
if(i==3){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching pentagrams:
Presult<-Pfreq[which(Pfreq$query==gram),]
# Sort by frequency:
Presult<-Presult[order(-Presult$count),]

# Reduce 1st test sentence to last 3 words:

for(i in 2:0) {
num<-(length(input[[j]])-i)
string<-input[[j]][num]
if(i==2){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching quadrigrams:
Qresult<-Qfreq[which(Qfreq$query==gram),]
# Sort by frequency:
Qresult<-Qresult[order(-Qresult$count),]

# Reduce 1st test sentence to last 2 words:

for(i in 1:0) {
num<-(length(input[[j]])-i)
string<-input[[j]][num]
if(i==1){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching trigrams:
Tresult<-Tfreq[which(Tfreq$query==gram),]
# Sort by frequency:
Tresult<-Tresult[order(-Tresult$count),]

# Reduce 1st test sentence to last word:

num<-(length(input[[j]]))
gram<-input[[j]][num]

# Get a DF with matching bigrams:
Bresult<-Bfreq[which(Bfreq$query==gram),]
# Sort by frequency:
Bresult<-Bresult[order(-Bresult$count),]

## Save results to latex files for reference ##
fileName=paste("results",j)
Btable<-xtable(Bresult[,1:2])
Ptable<-xtable(Presult[,1:2])
Qtable<-xtable(Qresult[,1:2])
Ttable<-xtable(Tresult[,1:2])
print.xtable(rbind(Ptable,Qtable,Ttable,Btable),type="latex", file=paste0(fileName,".tex"))
}

# put Ufreq in latex file.
UfreqSort<-Ufreq[order(-Ufreq$counts),]
Utable<-xtable(UfreqSort)
print.xtable(Utable,type="latex", file="UfreqSort.tex")

# Stop the clock
proc.time() - ptm # 1171.92
#=============================================#