## Combining Corpus Test ## 
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone")
fileName="testData1.txt"
lineNews1 <- readLines(fileName)
lineNews2 <- readLines(fileName)

corpus1<-makeCorpus(lineNews1)
corpus2<-makeCorpus(lineNews2)

myTDM1 <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer))
myTDM2 <- TermDocumentMatrix(corpus2, control = list(tokenize = TrigramTokenizer))

myTDMCombined <- c(myTDM1,myTDM2)

# Seems to work