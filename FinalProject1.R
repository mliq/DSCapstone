#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

#load in just 1000 lines of news dataset
require(tm)
fileName="en_US.news.txt"
lineNews <- readLines(fileName, n=1000)
corpus <- Corpus(VectorSource(lineNews))

# Now we have in TextDocument format with 7 metadata:
meta(corpus[[1]])
#Metadata:
#author       : character(0)
#datetimestamp: 2014-11-04 19:35:36
#description  : character(0)
#heading      : character(0)
#id           : 1
#language     : en
#origin       : character(0)

#Next, we make some adjustments to the text; 
#remove whitespace:
corpus1 <- tm_map(corpus, stripWhitespace)
inspect(corpus1) #don't see a big difference

#making everything lower case:
corpus2 <- tm_map(corpus1, content_transformer(tolower))
inspect(corpus2) #works

#remove stopwords
corpus3 <- tm_map(corpus2, removeWords, stopwords("english"))
inspect(corpus3) # ok the has been removed...

# Stemming
corpus4 <- tm_map(corpus3, stemDocument)
inspect(corpus4) # Looks stemmed.

## do these?
#removing punctuation? 
# corpus<- tm_map(corpus,removePunctuation)
#removing numbers ?
#corpus<- tm_map(corpus,removeNumbers)

##END DATA CLEANUP##

##BEGIN DATA ANALYSIS##

# First, we create something called a Term Document Matrix (TDM) 
# which is a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(corpus4)
inspect(tdm[1:20,]) #display top 20

# ok, a lot of stuff has hyphens. Guess I should remove those.
corpus5<- tm_map(corpus4,removePunctuation)
tdm2<- TermDocumentMatrix(corpus5)
inspect(tdm2[1:20,]) #display top 20

#ok, now it's all numbers, let's remove those too!
corpus6<- tm_map(corpus5,removeNumbers)
tdm3<- TermDocumentMatrix(corpus6)
inspect(tdm3[1:20,]) #display top 20
# ok now my problem is weird symbols: 
#Terms                    980 981 982 983 984 985 986 987 988 989 990 991 992
#â“iâ’m                   0   0   0   0   0   0   0   0   0   0   0   0   0
#â“oresteian              0   0   0   0   0   0   0   0   0   0   0   0   0
#â“reach                  0   0   0   0   0   0   0   0   0   0   0   0   0
#â€˜m                     0   0   0   0   0   0   0   0   0   0   0   0   0
#â€˜well                  0   0   0   0   0   0   0   0   0   0   0   0   0
#â€˜will                  0   0   0   0   0   0   0   0   0   0   0   0   0
#â€“                      0   0   0   0   0   0   0   0   0   0   0   0   0
#â€”                      0   0   0   0   0   0   0   0   0   0   0   0   0
#â€”found                 0   0   0   0   0   0   0   0   0   0   0   0   0
#â€\u009d

## WHAT'S NEXT?
## PERHAPS CLEAN THESE STRANGE CHARACTERS USING GSUB.
inspect(corpus6[10]) #here is an example with the odd data:
#â“  just tri  hit  hard someplaceâ” said rizzo  hit  pitch   opposit field  leftcenter â“iâ’m just   tri  make good contactâ”
#how was it orig?
inspect(corpus[10])
#Â“I was just trying to hit it hard someplace,Â” said Rizzo, who hit the pitch to the opposite field in left-center. Â“IÂ’m just up there trying to make good contact.Â”
#hmm.. so it has these strange letters from the start?