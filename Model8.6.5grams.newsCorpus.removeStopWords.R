# Start the clock!
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(xtable)
library(RWeka)
options("max.print"=1000000)

# FUNCTION DEFINITIONS #

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

# Tokenizer functions
PgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
QgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
UgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Input File processing

fileMunge<- function(x) {
text<-readLines(x, n=500000)
totalLines=length(text)
chunkSize=25000
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
text=fileMunge("en_US.news.txt")
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