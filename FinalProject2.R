#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

fileName="en_US.twitter.txt"
fileName2="en_US.news.txt"
fileName3="en_US.blogs.txt"
lineNews <- readLines(fileName, n=50000)

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

#Test:#

findAssocs(myTDM,"mother day",0)

findAssocs(myTDM,"thank follow",0)

findAssocs(myTDM,"happi birthday",0)

rm(corpus6)

## TRIM? ##
trimTDM<- removeSparseTerms(myTDM, .99)
trimTDM
# <<TermDocumentMatrix (terms: 80, documents: 50000)>>
# Non-/sparse entries: 82690/3917310
# Sparsity           : 98%
# Maximal term length: 8
# Weighting          : term frequency (tf)
# Decreases memory size to 3.9/50.9 MB = 7.6%


#---BELOW CAN BE DELETED--#

# # TRY THIS # Did not work
# sink("thank follow.txt")
# library(gamlr)
# findAssocsBig <- function(u, term, corlimit){
#   suppressWarnings(x.cor <-  corr(t(u[ !u$dimnames$Terms == term, ]),        
#                                          as.matrix(t(u[  u$dimnames$Terms == term, ]))  ))  
#   x <- sort(round(x.cor[(x.cor[, term] > corlimit), ], 2), decreasing = TRUE)
#   return(x)
# }
# findAssocsBig(txtTdmBi, "thank follow", 0.15)
#--#


## TRIM? ##
# trimTDM<- removeSparseTerms(myTDM, .01)
# trimTDM
# <<TermDocumentMatrix (terms: 0, documents: 50000)>>
# Non-/sparse entries: 0/0
# Sparsity           : 100%
# Maximal term length: 0
# Weighting          : term frequency (tf)

## TOKEN ANALYSIS ##

# MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the corpus.
#tdm<- TermDocumentMatrix(corpus6)
#dtm<- DocumentTermMatrix(corpus6)
#dtm

rm(corpus6)

##----##
# shrink, needed? #
# library(slam)
# txtTdmBi2 <- rollup(txtTdmBi, 2, na.rm=TRUE, FUN = sum)
# ALSO:
# ph.DTM2 <- removeSparseTerms(ph.DTM, 0.99999)

# Now use a lapply function to calculate the associated words for every item in the vector of terms in the term-document matrix. The vector of terms is most simply accessed with txtTdmBi$dimnames$Terms. For example txtTdmBi$dimnames$Terms[[1005]] is "foreign investment".

# Here I've used llply from the plyr package so we can have a progress bar (comforting for big jobs), but it's basically the same as the base lapply function.
findAssocs(myTDM, "case of", 0.5)
findAssocs(myTDM, "added today", 0.5)

# library(plyr)
# dat <- llply(txtTdmBi$dimnames$Terms, function(i) findAssocs(txtTdmBi, i, 0.5), .progress = "text" )


###--REFERENCE--###

## Tester ##
# sink('analysis-output.txt')
# inspect(dtm[,1:10])
# sink()

## Check if word exists ##
# 'hello' %in% Terms(dtm)