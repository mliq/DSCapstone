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
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

# TrigramTokenizer function
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# Make Corpus, Transform, Make Trigram TDM
makeTriTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}

## DATA MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#

# First I use a Unix Shell command to split the text file into 20,000 lines each chunks:
# split --numeric-suffixes -a 4 --lines=20000 en_US.twitter.txt twitter

#initialize a list to hold our data
twit<-list()

# Read in text, break into chunks
fileName="en_US.twitter.txt"
text<-readLines(fileName)

totalLines=length(text)
chunkSize=20000
chunks=totalLines/chunkSize
remainder = chunks %% 1
wholeChunks = chunks-remainder

i=1
line=1
while (i<=wholeChunks){
end=line+chunkSize-1
twit[[i]]<-text[line:end]
line=end+1
i=i+1
}
twit[[i]]<-text[line:totalLines]


# Text Transformations to remove odd characters #
twit=lapply(twit,FUN=iconv, to='ASCII', sub=' ')
twit=lapply(twit,FUN= function(x) gsub("'{2}", " ",x))
twit=lapply(twit,FUN= function(x) gsub("[0-9]", " ",x))
	
# Build TDM with tri-grams.

triTDM <- makeTriTDM(twit)

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

# Set first two words as an attribute, the trigram prediction query pair of words
freqTable$triquery <- sapply(words,FUN=function(x) paste(x[1],x[2]))

# Set each word of trigram as an attribute for future use
freqTable$one <- sapply(words,FUN=function(x) paste(x[1]))
freqTable$two <- sapply(words,FUN=function(x) paste(x[2]))
freqTable$three <- sapply(words,FUN=function(x) paste(x[3]))

# 3. INPUT MUNGING
#=============================================#
## INPUT MUNGING ##
# i. Take an input:
input<-list("The guy in front of me just bought a pound of bacon, a bouquet, and a case of","You're the reason why I smile everyday. Can you follow me please? It would mean the","Hey sunshine, can you follow me and make me the","Very early observations on the Bills game: Offense still struggling but the","Go on a romantic date at the","Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my","Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some","After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little","Be grateful for the good times and keep the faith during the","If this isn't the cutest thing you've ever seen, then you must be")

# ii. Perform Transformations.
input<-makeCorpus(input)
input<-lapply(input, FUN=function(x){as.character(x[1])})

# iii. Reduce to last two words
input<-lapply(input,FUN=function(x){unlist(strsplit(x,"\\s+"))})
two<-lapply(input,FUN=length)
one<-lapply(two,FUN=function(x){x-1})
# iv. set querying bigrams and unigrams we will search for in trigrams and bigrams respectively
bigram<-list(lapply(1:length(input),FUN=function(x){paste(input[[x]][one[[x]]],input[[x]][two[[x]]])}))
bigram<-bigram[[1]]
unigram<-list(lapply(1:length(input),FUN=function(x){paste(input[[x]][two[[x]]])}))
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
