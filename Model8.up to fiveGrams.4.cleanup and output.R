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
rm(Utdm)
gc()
Ufreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Btdm)
rm(Btdm)
gc()
Bfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Ttdm)
rm(Ttdm)
gc()
Tfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Qtdm)
rm(Qtdm)
gc()
Qfreq<-data.frame(grams=names(counts), counts=counts)
counts=row_sums(Ptdm)
rm(Ptdm)
gc()
Pfreq<-data.frame(grams=names(counts), counts=counts)
rm(counts)
gc()

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
# Start the clock!
ptm <- proc.time()

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

## Save results to file ##
fileName=paste("results",j)
# sink(fileName)
# print("================================")
# print("   Presult   ")
# print(Presult[,1:2])
# print("================================")
# print("   Qresult   ")
# print(Qresult[,1:2])
# print("================================")
# print("   Tresult   ")
# print(Tresult[,1:2])
# print("================================")
# print("   Bresult   ")
# print(Bresult[,1:2])
# print("================================")
# sink()

# Put in latex file for easier reading
library(xtable)
Btable<-xtable(Bresult[,1:2])
Ptable<-xtable(Presult[,1:2])
Qtable<-xtable(Qresult[,1:2])
Ttable<-xtable(Tresult[,1:2])
print.xtable(rbind(Ptable,Qtable,Ttable,Btable),type="latex", file=paste0(fileName,".tex"))
}

# Stop the clock
proc.time() - ptm # 1.34 secs, wow! 

# Whole script: 1867.46 (31 minutes) ALMOST all RAM used! could not do more.
#=============================================#

#BresultRefined<-Bresult[which(Bresult$counts>=3),]
#BresultRefined<-BresultRefined[order(-BresultRefined$count),]

#=============================================#

Below uses if statements, but for now lets just get all results into files.
#=============================================#
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
# Error in which(Tresult$counts >= 3) : object 'Tresult' not found
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
object.size(Bfreq)
790626128 bytes = 790MB
object.size(Ufreq)
21492600 bytes = 21MB
object.size(Qfreq)
2766943008 bytes = 2.8GB
object.size(Pfreq)
3038129744 bytes = 3GB

TDMs add another 1 GB, could remove those.

#Since i'm ignoring terms which appear less than 3 times ANYWAY, why not just eliminate them from DB?

UfreqSort<-Ufreq[order(-Ufreq$counts),]
options("max.print"=1000000)
sink("UfreqSort")
print(UfreqSort)
sink()

library(xtable)
UfreqSort<-Ufreq[order(-Ufreq$counts),]
Utable<-xtable(UfreqSort)
print.xtable(Utable,type="html",file="UfreqSort.html")
print.xtable(Utable,type="latex", file="UfreqSort.tex")