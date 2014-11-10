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
corpus <- Corpus(VectorSource(cleanData3))

## TOKENIZATION ## 

# REMOVE WHITESPACE:
corpus1 <- tm_map(corpus, stripWhitespace)
# inspect(corpus1) #don't see a big difference
rm(corpus)
# LOWERCASE:
corpus2 <- tm_map(corpus1, content_transformer(tolower))
# inspect(corpus2) #works
rm(corpus1)
# REMOVE STOPWORDS
corpus3 <- tm_map(corpus2, removeWords, stopwords("english"))
# inspect(corpus3) # ok the has been removed...
rm(corpus2)
# STEMMING
corpus4 <- tm_map(corpus3, stemDocument)
# inspect(corpus4) # Looks stemmed.
rm(corpus3)
# REMOVE PUNCTUATION
corpus5<- tm_map(corpus4,removePunctuation)
rm(corpus4)
# REMOVE NUMBERS
corpus6<- tm_map(corpus5,removeNumbers)
rm(corpus5)
## END TOKENIZATION ##

# Build TDM with uni-, bi- and tri-grams.
library(rJava) # Is this really needed?
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
myTDM <- TermDocumentMatrix(corpus6, control = list(tokenize = TrigramTokenizer))

myTDM

rm(corpus6)


## TRIM? ##
trimTDM<- removeSparseTerms(myTDM, .99)
trimTDM

# -- #
#Test:#
system.time(findAssocs(myTDM,"mother day",0.20), gcFirst = TRUE)

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