## CORPUS MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone")
fileName="testData1.txt"
lineNews <- readLines(fileName)

## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.
cleanData<-iconv(lineNews, to='ASCII', sub=' ')
# Replace numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)

## MAKE CORPUS ##
library(tm)
makeCorpus <- function(x) corpus <- {
	corpus<-Corpus(VectorSource(x))
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	corpus <- tm_map(corpus, stemDocument)
	corpus<- tm_map(corpus,removePunctuation)
	corpus<- tm_map(corpus,removeNumbers)
}
corpus<-makeCorpus(lineNews)

# Build TDM with tri-grams.
library(rJava) # Is this really needed?
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

myTDM <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

# 2. Isolate bigrams and unigrams within trigrams 
#=============================================#

# Get total frequency in corpus of each trigram
gramCount<-rowSums(as.matrix(myTDM))

# Create dataframe frequency table
freqTable <- data.frame(gram=names(gramCount),count=gramCount,stringsAsFactors=FALSE)

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
input<-"my favorite band is talking"

# ii. Perform Transformations.
input<-makeCorpus(input)
input<-as.character(input[[1]][1])

# iii. Reduce to last two words
input<-unlist(strsplit(input,"\\s+"))
two<-length(input)
one<-two-1
# iv. set querying bigrams and unigrams we will search for in trigrams and bigrams respectively
bigram<-paste(input[one],input[two])
unigram<-paste(input[two])

# 4. FIND PREDICTION MATCHES
#=============================================#

#TRIGRAMS:

# Find trigrams where first two words match and put in matches list
trimatches <- freqTable[freqTable$triquery == bigram,]
bimatch1 <- freqTable[freqTable$one == unigram,]
bimatch2 <- freqTable[freqTable$two == unigram,]

# Print out results:
for (i in 1:nrow(trimatches)){print(paste("Trigram prediction ",i,": ",trimatches$three[i]))}
for (i in 1:nrow(bimatch1)){print(paste("Bigram prediction ",i,": ",bimatch1$two[i]))}
k=nrow(bimatch1)
for (i in 1:nrow(bimatch1)){
k=k+1
print(paste("Bigram prediction ",k,": ",bimatch1$two[i]))}

# Next: Instead of printing, put these results in a frequency table and rank them as such.
matches<-c(trimatches$three,bimatch1$two,bimatch2$one)
matchCorpus<-makeCorpus(matches) # (Corpus(VectorSource(matches)) caused very similar terms to be considered separately like singular and plural)
matchTDM<-TermDocumentMatrix(matchCorpus)

# Get total frequency in prediction corpus of each prediction
predCount<-rowSums(as.matrix(matchTDM))

# Create dataframe frequency table
predFreq <- data.frame(gram=names(predCount),count=predCount,stringsAsFactors=FALSE)
predFreq<-predFreq[order(-predFreq$count),]