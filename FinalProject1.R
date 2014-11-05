#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

#--- Here we have debate about the methods, I will go with the first one since none seem to fix anything.
#load in just 1000 lines of news dataset
fileName="en_US.news.txt"
lineNews <- readLines(fileName, n=1000)

## REMOVAL OF STRANGE CHARACTERS##
clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", lineNews)
clean_data <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", clean_data)
clean_data <- gsub("[\002\020\023\177\003]", "", clean_data)
clean_data <- gsub("™", "", clean_data)
clean_data <- gsub("˜", "", clean_data)
clean_data <- gsub("“", "", clean_data)
clean_data <- gsub("”", "", clean_data)
# ELIMINATE DASHES / HYPHENS
clean_data <- gsub("-", " ", clean_data)
## END REMOVAL OF STRANGE CHARACTERS##

## MAKE CORPUS ##
require(tm)
corpus <- Corpus(VectorSource(clean_data))

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

## CORPUS ANALYSIS ##

## MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(corpus6)
inspect(tdm[1:20,]) #display top 20
