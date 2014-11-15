# Start the clock!
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)

# FUNCTION DEFINITIONS #

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

# Tokenizer functions
library(RWeka)
PgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
QgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
UgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Input File processing

fileMunge<- function(x) {
text<-readLines(x, n=500000)
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
#=============================================#
#=============================================#

# DATA MUNGING #

# 1. Corpus, transformations, and TDM Creation
#=============================================#

# Read, chunk, parse data, then make corpus, do transformations, make TDM of tri-grams:
text=fileMunge("en_US.twitter.txt")
twit<-makeCorpus(text)
Ttdm<- TermDocumentMatrix(twit, control = list(tokenize = TgramTokenizer))
gc()
Qtdm<- TermDocumentMatrix(twit, control = list(tokenize = QgramTokenizer))
gc()
Btdm<- TermDocumentMatrix(twit, control = list(tokenize = BgramTokenizer))
gc()
Utdm<- TermDocumentMatrix(twit, control = list(tokenize = UgramTokenizer))
gc()
Ptdm<- TermDocumentMatrix(twit, control = list(tokenize = PgramTokenizer))
gc()

# rm(twit)

# 
library(slam)
counts=row_sums(Utdm)
Ufreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Btdm)
Bfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Ttdm)
Tfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Qtdm)
Qfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Ptdm)
Pfreq<-data.frame(grams=names(counts), counts=counts)

# Get a DF with only terms appearing over x amount of times:
# Pfreq[which(Pfreq$counts>=5),]

#=============================================#
# PREDICTION #
#=============================================#

## Prediction Corpus Preparation ##

# isolate the query words from the predicted final word of each n-gram:
length(as.character(Pfreq$grams[1]))

test<-unlist(strsplit(as.character(Pfreq$grams[1]),"\\s+"))

Pfreq$words<-lapply(Pfreq$grams,FUN=function(x){unlist(strsplit(as.character(x),"\\s+"))})
Pfreq$query<-lapply(Pfreq$words,FUN=function(x){paste(x[1:4],collapse=' ')}) #not positive if i want this but for now...
Pfreq$result<-lapply(Pfreq$words,FUN=function(x){x[5]})

Qfreq$words<-lapply(Qfreq$grams,FUN=function(x){unlist(strsplit(as.character(x),"\\s+"))})
Qfreq$query<-lapply(Qfreq$words,FUN=function(x){paste(x[1:3],collapse=' ')})
Qfreq$result<-lapply(Qfreq$words,FUN=function(x){x[4]})

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

# First, by pentagrams

# Reduce 1st test sentence to last 4 words
for(i in 3:0) {
num<-(length(input[[1]])-i)
string<-input[[1]][num]
if(i==3){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching pentagrams:
Presult<-Pfreq[which(Pfreq$query==gram),]

# If no matches with count of 3 or greater found, move to quadrigrams

if(nrow(Pfreq[which(Presult$counts>=3),])==0){

# Reduce 1st test sentence to last 3 words:

for(i in 2:0) {
num<-(length(input[[1]])-i)
string<-input[[1]][num]
if(i==2){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching quadrigrams:
Qresult<-Qfreq[which(Qfreq$query==gram),]
}

# If no matches with count of 3 or greater found, move to trigrams

if(nrow(Qfreq[which(Qresult$counts>=3),])==0){

# Reduce 1st test sentence to last 2 words:

for(i in 1:0) {
num<-(length(input[[1]])-i)
string<-input[[1]][num]
if(i==1){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching trigrams:
Tresult<-Tfreq[which(Tfreq$query==gram),]
}

# If none, move to bigrams

if(nrow(Tfreq[which(Tresult$counts>=3),])==0){

# Reduce 1st test sentence to last word:

num<-(length(input[[1]]))
gram<-input[[1]][num]

# Get a DF with matching bigrams:
Bresult<-Bfreq[which(Bfreq$query==gram),]
}

BresultRefined<-Bresult[which(Bresult$counts>=3),]
BresultRefined<-BresultRefined[order(-BresultRefined$count),]

# get all of our potential predictions and find their frequency in the unigram table
# So for now, this doesn't work to great, but my hope is that with a larger data size, trigrams will work out again...
# may also need to eliminate stopwords at some point...

# Stop the clock
proc.time() - ptm #  1867.46 (31 minutes) ALMOST all RAM used! could not do more.
#=============================================#
#=============================================#
