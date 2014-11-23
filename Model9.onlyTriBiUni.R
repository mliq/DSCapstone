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

process<- function(output) {
# Text Transformations to remove odd characters #
# replace APOSTROPHES OF 2 OR MORE with space - WHY??? that never happens..
	# output=lapply(output,FUN= function(x) gsub("'{2}", " ",x))
# Replace numbers with spaces... not sure why to do that yet either.
	# output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
# Erase commas.
output=lapply(output,FUN=function(x) gsub(",?", "", x))
# Erase ellipsis
output=lapply(output,FUN=function(x) gsub("\\.{3,}", "", x))
# Erase colon
output=lapply(output,FUN=function(x) gsub("\\:", "", x))
##### SENTENCE SPLITTING AND CLEANUP
# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
# ok here is where you change structure fundamentally... 
output<-strsplit(output[[1]],"[\\.]{1}")
output<-strsplit(unlist(output),"\\?+")
output<-strsplit(unlist(output),"\\!+")
output<-strsplit(unlist(output),"\\-+")
# Split also on parentheses
output<-strsplit(unlist(output),"\\(+")
output<-strsplit(unlist(output),"\\)+")
# split also on quotation marks
output<-strsplit(unlist(output),"\\\"")
# remove spaces at start and end of sentences:
output<-lapply(output,FUN=function(x) gsub("^\\s+", "", x))
output<-lapply(output,FUN=function(x) gsub("\\s+$", "", x))
# Replace ~ and any whitespace around with just one space
output<-lapply(output,FUN=function(x) gsub("\\s*~\\s*", " ", x))
# Replace forward slash with space
output<-lapply(output,FUN=function(x) gsub("\\/", " ", x))
# Replace + signs with space
output<-lapply(output,FUN=function(x) gsub("\\+", " ", x))
# Eliminate empty and single letter values (more?)
output[which(nchar(unlist(unlist(output)))==1)]=NULL
output[which(nchar(unlist(unlist(output)))==0)]=NULL
# so original has chunks where each is a list
# after all this instead i have many chunks. Which obviously will slow shit down... 
output=unlist(output)
# Running this puts all back in a single list. This may also be bad though because it is too long. Can I break it to chunks again?
}

# Tokenizer functions
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
UgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Input File processing

fileMunge<- function(x) {
text<-readLines(x, n=1000)
totalLines=length(text)
chunkSize=100
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
}
#=============================================#
#=============================================#

# DATA MUNGING #

# 1. Corpus, transformations, and TDM Creation
#=============================================#

# Read, chunk, parse data, then make corpus, do transformations, make TDM of tri-grams:
text=fileMunge("en_US.blogs.txt")
text=process(text)

blog<-makeCorpus(text)

Ttdm<- TermDocumentMatrix(blog, control = list(tokenize = TgramTokenizer))
gc()
Btdm<- TermDocumentMatrix(blog, control = list(tokenize = BgramTokenizer))
gc()
Utdm<- TermDocumentMatrix(blog, control = list(tokenize = UgramTokenizer))
gc()

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