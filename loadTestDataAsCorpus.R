#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

fileName="testData.txt"
testData <- readLines(fileName)

## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.
TcleanData<-iconv(testData, to='ASCII', sub=' ')
# Replace numbers and ''
TcleanData2 <- gsub("'{2}", " ", TcleanData)
TcleanData3 <- gsub("[0-9]", " ", TcleanData2)

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
Ttdm<- TermDocumentMatrix(Tcorpus6)
Tdtm<- DocumentTermMatrix(Tcorpus6)
Tdtm

# Compare
# Ah, findAssocs does take char vector, NOT another TDM.
# findAssocs(x, terms, corlimit)
# x	
# A DocumentTermMatrix or a TermDocumentMatrix.
# terms	
# a character vector holding terms.
# corlimit	
# a numeric vector (of the same length as terms; recycled otherwise) for the (inclusive) lower correlation limits of each term in the range from zero to one.

findAssocs(tdm,TcleanData3,corlimit=0.5)
#numeric 0 is returned...
# Let's try with something from the dataset
cleanData[1]
findAssocs(dtm,cleanData[1],.5)

#None of that works, but this does:
findAssocs(tdm,"dog",corlimit=0.5)
#So the problem may be that I'm feeding it a vector where the words are not broken up.
TSplit<-strsplit(TcleanData3," ")
findAssocs(tdm,TSplit,corlimit=0.5)