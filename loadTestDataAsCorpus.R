#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

fileName="testData.txt"
testData <- readLines(fileName)

## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.
TcleanData<-iconv(testData, to='ASCII', sub=' ')
# Replace numbers and ''
TcleanData2 <- gsub("'{2}", " ", cleanData)
TcleanData3 <- gsub("[0-9]", " ", cleanData2)

## MAKE CORPUS ##
require(tm)
Tcorpus <- Corpus(VectorSource(TcleanData3))

## TOKENIZATION ## 

# REMOVE WHITESPACE:
Tcorpus1 <- tm_map(Tcorpus, stripWhitespace)
inspect(Tcorpus1) #don't see a big difference

# LOWERCASE:
Tcorpus2 <- tm_map(Tcorpus1, content_transformer(tolower))
inspect(Tcorpus2) #works

# REMOVE STOPWORDS
Tcorpus3 <- tm_map(Tcorpus2, removeWords, stopwords("english"))
inspect(Tcorpus3) # ok the has been removed...

# STEMMING
Tcorpus4 <- tm_map(Tcorpus3, stemDocument)
inspect(Tcorpus4) # Looks stemmed.

# REMOVE PUNCTUATION
Tcorpus5<- tm_map(Tcorpus4,removePunctuation)

# REMOVE NUMBERS
Tcorpus6<- tm_map(Tcorpus5,removeNumbers)

## END TOKENIZATION ##

## TOKEN ANALYSIS ##

# MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the Tcorpus.
tdm<- TermDocumentMatrix(Tcorpus6)
dtm<- DocumentTermMatrix(Tcorpus6)
dtm