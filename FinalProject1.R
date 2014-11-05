#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

fileName="en_US.news.txt"
lineNews <- readLines(fileName, n=2000)

## REMOVAL OF STRANGE CHARACTERS##
#Try iconv(mytext, to='ASCII', sub=' ') to replace the unicode characters with spaces.
cleanData<-iconv(lineNews, to='ASCII', sub=' ')
#Cool ! ?
#Key to fix : line 1854 " ˆšBarbara Trelstad  "
#lineNews[1854]
#cleanData[1854]

# new issue is numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)

## MAKE CORPUS ##
require(tm)
corpus <- Corpus(VectorSource(cleanData3))

# MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(corpus)
inspect(tdm[1:20,]) #display top 20


# === ABOVE IS REASONABLE, THOUGH STILL ISSUE OF 'em and 'cause other such slang #

# Output to text for testing #
sink('analysis-output.txt')
cleanData
sink()
# END TESTER #

#
clean_data <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", lineNews)
clean_data <- gsub("'", "", clean_data)
clean_data <- gsub("™", " ", clean_data)
clean_data <- gsub("\"", " ", clean_data)
# good here but now numbers rule, also, hyphens
clean_data <- gsub("[0-9]", " ", clean_data)
clean_data <- gsub("$", " ", clean_data)
clean_data <- gsub("-", " ", clean_data)
# here we still have () $$$ % : . 
clean_data <- gsub("[()$%.*@^/šˆ`¨´?˜]", " ", clean_data)
clean_data <- gsub("š", " ", clean_data)
# note for '\' gsub in r you need to put "\\\\" http://r.789695.n4.nabble.com/gsub-with-unicode-and-escape-character-td3672737.html



#clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", lineNews)
# ok but not some others have appeared again... so somethinghere screws shit up.

# TESTER 2: http://stackoverflow.com/questions/24920396/r-corpus-is-messing-up-my-utf-8-encoded-text #
dtm <- DocumentTermMatrix(corpus)
Terms(dtm)

# TESTER #
clean_data

corpus <- Corpus(VectorSource(clean_data))
tdm<- TermDocumentMatrix(corpus)
inspect(tdm[1:20,]) #display top 20

# Output to text for testing #
sink('analysis-output.txt')
inspect(corpus)
sink()
# END TESTER #

#--GO STEP BY STEP ADDING BELOW -- #
clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", lineNews)
#clean_data <- gsub("[\002\020\023\177\003]", "", clean_data)
clean_data <- gsub("™", " ", clean_data)
clean_data <- gsub("˜", " ", clean_data)
clean_data <- gsub("“", " ", clean_data)
clean_data <- gsub("”", " ", clean_data)
#clean_data <- gsub("[:punct:]", "", clean_data)
# ELIMINATE DASHES / HYPHENS
clean_data <- gsub("-", " ", clean_data)
# error: clean_data <- gsub("\\", " ", clean_data)
## END REMOVAL OF STRANGE CHARACTERS##




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
#corpus5<- tm_map(corpus4,removePunctuation)

# REMOVE NUMBERS
corpus6<- tm_map(corpus5,removeNumbers)

## END TOKENIZATION ##

## CORPUS ANALYSIS ##

## MAKE CORPUS ##
require(tm)
corpus <- Corpus(VectorSource(cleanData))

# MAKE TERM DOCUMENT MATRIX (TDM) - a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(corpus)
inspect(tdm[1:20,]) #display top 20
