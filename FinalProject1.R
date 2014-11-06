#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

fileName="en_US.news.txt"
lineNews <- readLines(fileName, n=2000)

### NOTE: I SHOULD REMOVE PROFANITY AS WELL SOMEWHERE ###

## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.
cleanData<-iconv(lineNews, to='ASCII', sub=' ')
# Replace numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)

## MAKE CORPUS ##
require(tm)
corpus <- Corpus(VectorSource(cleanData3))

## TOKENIZATION ## 

# REMOVE WHITESPACE:
corpus1 <- tm_map(corpus, stripWhitespace)
inspect(corpus1) #don't see a big difference

# LOWERCASE:
corpus2 <- tm_map(corpus1, content_transformer(tolower))
inspect(corpus2) #works

# REMOVE STOPWORDS
corpus3 <- tm_map(corpus2, removeWords, stopwords("english"))
inspect(corpus3) # ok the has been removed...

# STEMMING
corpus4 <- tm_map(corpus3, stemDocument)
inspect(corpus4) # Looks stemmed.

# REMOVE PUNCTUATION
corpus5<- tm_map(corpus4,removePunctuation)

# REMOVE NUMBERS
corpus6<- tm_map(corpus5,removeNumbers)

## END TOKENIZATION ##

## TOKEN ANALYSIS ##

# MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(corpus6)
dtm<- DocumentTermMatrix(corpus6)
dtm

## WHAT NEXT?
#SOME COMMANDS:
#findFreqTerms(x = tdm, lowfreq=58, highfreq=Inf)
#findAssocs(x=tdm,term="also",corlimit=0.1)

# 1. take a sentence and iterate through the words

# 2. Look up associated words 

# 3. Give a score to each association with higher score to the words near the blank. 

## POSSIBILITIES:

# 1. REMOVE SPARSE TERMS
# TDM.common = removeSparseTerms(TDM, 0.1)
# dim(TDM)
# [1] 18651   182
# dim(TDM.common)
# [1]  71 182

