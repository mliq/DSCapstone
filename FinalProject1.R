#Final Project 1
#===================
#For now just using news dataset - first 1000

##BEGIN DATA CLEANUP##

#Set working directory
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
#Load in news dataset
fileName="en_US.news.txt"
con=file(fileName,open="r")
lineNews=readLines(con) 
close(con)
rm(fileName,con)

#now we have lineNews, a 77259 long character list
#let's edit down to 1000 lines for now.
aset<-lineNews[1:1000]

#load to corpus
require(tm)
txt<-VectorSource(aset)
txt.corpus<-Corpus(txt)
inspect(txt.corpus)

#Next, we make some adjustments to the text; 
#making everything lower case, removing punctuation, removing numbers,
# and removing common English stop words. 
#The ‘tm map’ function allows us to apply transformation functions to a corpus.
txt.corpus<- tm_map(txt.corpus,tolower)
txt.corpus<- tm_map(txt.corpus,removePunctuation)
txt.corpus<- tm_map(txt.corpus,removeNumbers)
txt.corpus<- tm_map(txt.corpus,removeWords,stopwords("english"))

#Next we perform stemming, which truncates words (e.g., “compute”, “computes” & 
#“computing” all become “comput”). 
#However, we need to load the ‘SnowballC’ package (Bouchet-Valat, 2013) 
#which allows us to identify specific stem elements using the ‘tm map’ 
#function of the ‘tm’ package.
require(SnowballC)
txt.corpus<- tm_map(txt.corpus, stemDocument)
detach("package:SnowballC")

#strip whitespace
txt.corpus<-tm_map(txt.corpus, stripWhitespace)

##END DATA CLEANUP##

##BEGIN DATA ANALYSIS##

# First, we create something called a Term Document Matrix (TDM) 
# which is a matrix of frequency counts for each word used in the corpus.
tdm<- TermDocumentMatrix(txt.corpus)
inspect(tdm[1:20,]) #display top 20
#ERROR: TDM not created: 
#Error: inherits(doc, "TextDocument") is not TRUE
# The problem is that the functions tolower and trim won't necessarily 
# return TextDocuments (it looks like the older version may have automatically 
# done the conversion). They instead return characters and the DocumentTermMatrix
# isn't sure how to handle a corpus of characters.
#So you could change to
#corpus_clean <- tm_map(news_corpus, content_transformer(tolower))
#Or you can run
#corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
#after all of your non-standard transformations (those not in 
# getTransformations()) are done and just before you create the DocumentTermMatrix. That should
# make sure all of your data is in PlainTextDocument and should make DocumentTermMatrix happy.
# http://stackoverflow.com/questions/24191728/documenttermmatrix-error-on-corpus-argument

txt.corpus <- tm_map(txt.corpus, PlainTextDocument)


#-------
##Issues
# what is this?
# [[997]]
# [1]  trillion projected overall cost  alzheimerâ€™s patients 
# what about single letters?

