# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)

# FUNCTION DEFINITIONS #

# Make Corpus and do transformations only
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

# TrigramTokenizer function
library(RWeka)
gramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Make Corpus, Transform, Make Trigram TDM
makeTriTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<- TermDocumentMatrix(corpus, control = list(tokenize = gramTokenizer))
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
myTDM <- makeTriTDM(twit)
rm(twit)
gc()

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
