#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
# setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
# fileName="en_US.twitter.txt"
# fileName2="en_US.news.txt"
# fileName3="en_US.blogs.txt"

setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone")
fileName="testData1.txt"
lineNews <- readLines(fileName)

### NOTE: I SHOULD REMOVE PROFANITY AS WELL SOMEWHERE ###

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

# Build TDM with uni-, bi- and tri-grams.
library(rJava) # Is this really needed?
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

myTDM <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

myTDM

## TEST ##
testCorpus<-makeCorpus("talking heads")
testTDM<-TermDocumentMatrix(testCorpus, control = list(tokenize = TrigramTokenizer))

findAssocs(myTDM,testCorpus[[1]][1]$content,0.0)


## TRIM? ##
trimTDM<- removeSparseTerms(myTDM, .99)
trimTDM

# -- #

## MODEL ##
# 1. Must convert input to same format (lower, stemmed.)

## END MODEL ##

#Test:#

system.time(findAssocs(myTDM,"Talking heads",0.20), gcFirst = TRUE)
findAssocs(myTDM,"talking heads",0.0)

findAssocs(myTDM,"mother day",0.20)
findAssocs(trimTDM,"mother day",0.20)

##--##

# <<TermDocumentMatrix (terms: 530551, documents: 50000)>>
# Non-/sparse entries: 893210/26526656790
# Sparsity           : 100%
# Maximal term length: 93
# Weighting          : term frequency (tf)

# TEST #
findAssocs(myTDM, "case of", 0)
findAssocs(myTDM, "added today", 0)
'added today' %in% Terms(myTDM)
findFreqTerms(x = myTDM, lowfreq=100, highfreq=Inf)

#OK, seems to work, just need to expand the corpus i guess


# findAssocs(myTDM,"thank follow",0)

# findAssocs(myTDM,"happi birthday",0)

rm(corpus6)