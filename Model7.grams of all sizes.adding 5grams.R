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
text<-readLines(x, n=100000)
totalLines=length(text)
chunkSize=10000
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

# Stop the clock
proc.time() - ptm # 95 secs 
#=============================================#
# PREDICTION #
#=============================================#

## Prediction Corpus Preparation ##

# isolate the query words from the predicted final word of each n-gram:
length(as.character(Pfreq$grams[1]))

test<-unlist(strsplit(as.character(Pfreq$grams[1]),"\\s+"))

Pfreq$words<-lapply(Pfreq$grams,FUN=function(x){unlist(strsplit(as.character(x),"\\s+"))})
Pfreq$query<-lapply(Pfreq$words,FUN=function(x){x[1:4]})
Pfreq$result<-lapply(Pfreq$words,FUN=function(x){x[5]})

# now let's see if this is too slow...

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

# Reduce 1st sentence to last 4 words
for(i in 3:0) {
num<-(length(input[[1]])-i)
string<-input[[1]][num]
if(i==3){gram=string}
else{gram<-paste(gram,string)}
}

# Get a DF with matching pentagrams:
Presult<-Pfreq[which(Pfreq$grams==gram),]

# If none, move to quadrigrams

for(i in 2:0) {
num<-(length(input[[1]])-i)
string<-input[[1]][num]
if(i==2){gram=string}
else{gram<-paste(gram,string)}
}

if(nrow(Presult)==0){
# Get a DF with matching quadrigrams:
Qresult<-Qfreq[which(Qfreq$grams==gram),]
}

# If none, move to trigrams

if(nrow(Qresult)==0){
# Get a DF with matching trigrams:
Tresult<-Tfreq[which(Tfreq$grams==gram),]
}

# If none, move to bigrams

if(nrow(Tresult)==0){
# Get a DF with matching trigrams:
Bresult<-Bfreq[which(Bfreq$grams==gram),]
}

#=============================================#
#=============================================#

# 45 seconds with 100,000 files broken to 10k each chunks.

# now break up trigrams to bi and uni grams? or hell, just re-run the tokenizer if it was so easy right?


#--#
# news<-fileMunge("en_US.news.txt")
# newTDM <- makeTriTDM(news)
# triTDM<-c(triTDM,newTDM)
# rm(news)
# rm(newTDM)
# gc()

# blog<-fileMunge("en_US.blogs.txt")
# blogTDM <- makeTriTDM(blog)
# triTDM<-c(triTDM,blogTDM)
# rm(blog)
# rm(blogTDM)
# gc()

# going further crashes without some removal.(27 million rows)
# library(tm)
triTDM<-removeSparseTerms(myTDM,0.96)
gc()
# 2. Isolate bigrams and unigrams within trigrams 
#=============================================#

# Get total frequency in corpus of each trigram
library(slam)
gramCount<-as.matrix(row_sums(triTDM))

# Create dataframe frequency tables
freqTable <- data.frame(gram=dimnames(gramCount)[[1]],count=gramCount,stringsAsFactors=FALSE)

# sort descending the frequency tables
freqTable<-freqTable[order(-freqTable$count),]

# Split corpus trigrams up to words
words <- strsplit(freqTable$gram," ")

# Set first three words as an attribute, the quadrigram prediction query pair of words
freqTable$quadquery <- sapply(words,FUN=function(x) paste(x[1],x[2],x[3]))

# Set second two words as an attribute, the trigram prediction query pair of words
freqTable$triquery <- sapply(words,FUN=function(x) paste(x[2],x[3]))

# Set each word of quadgram as an attribute for future use
freqTable$one <- sapply(words,FUN=function(x) paste(x[1]))
freqTable$two <- sapply(words,FUN=function(x) paste(x[2]))
freqTable$three <- sapply(words,FUN=function(x) paste(x[3]))
freqTable$four <- sapply(words,FUN=function(x) paste(x[4]))

# 3. INPUT MUNGING
#=============================================#
## INPUT MUNGING ##
# i. Take an input:
input<-readLines("Quiz2.txt")

# ii. Perform Transformations.
input<-makeCorpus(input)
input<-lapply(input, FUN=function(x){as.character(x[1])})

#This is a mistake ? # iii. Reduce to last three words
input<-lapply(input,FUN=function(x){unlist(strsplit(x,"\\s+"))})
three<-lapply(input,FUN=length)
two<-lapply(input,FUN=function(x){x-1})
one<-lapply(two,FUN=function(x){x-2})

# Updated to here #
# iv. set querying bigrams and unigrams we will search for in trigrams and bigrams respectively
bigram<-list(lapply(1:length(input),FUN=function(x){paste(input[[x]][two[[x]]],input[[x]][three[[x]]])}))
bigram<-bigram[[1]]
unigram<-list(lapply(1:length(input),FUN=function(x){paste(input[[x]][three[[x]]])}))
unigram<-unigram[[1]]

# 4. FIND PREDICTION MATCHES
#=============================================#

#TRIGRAMS:

# Find trigrams where first two words match and put in matches list
trimatches<-lapply(1:length(bigram), FUN=function(x){freqTable[freqTable$triquery == bigram[[x]],]})
bimatch1 <- lapply(1:length(unigram), FUN=function(x){freqTable[freqTable$one == unigram[[x]],]})
bimatch2 <- lapply(1:length(unigram), FUN=function(x){freqTable[freqTable$two == unigram[[x]],]})

# Put these results in a frequency table and rank them as such.
matches<-lapply(1:length(input), FUN=function(x){c(trimatches[[x]]$three,bimatch1[[x]]$two,bimatch2[[x]]$one)})
matchCorpus<-lapply(1:length(matches),FUN=function(x){makeCorpus(matches[[x]])}) # (Corpus(VectorSource(matches)) caused very similar terms to be considered separately like singular and plural)
matchTDM<-lapply(1:length(matchCorpus),FUN=function(x){TermDocumentMatrix(matchCorpus[[x]])})

# Get total frequency in prediction corpus of each prediction
predCount<-lapply(1:length(matchTDM),FUN=function(x){rowSums(as.matrix(matchTDM[[x]]))})

# Create dataframe frequency table
predFreq <- lapply(1:length(predCount),FUN=function(x){data.frame(gram=names(predCount[[x]]),count=predCount[[x]],stringsAsFactors=FALSE)})
predFreq<-lapply(1:length(predFreq),FUN=function(x){predFreq[[x]][order(-predFreq[[x]]$count),]})
# If predictions are more than 10, reduce to 10
predFreq<-lapply(1:length(predFreq),FUN=function(x){if(nrow(predFreq[[x]])>10){predFreq[[x]]<-predFreq[[x]][1:10,]}})

sink('predictions.txt')
predFreq
sink()
sink('trimatches.txt')
trimatches
sink()
gc()
# Stop the clock
proc.time() - ptm