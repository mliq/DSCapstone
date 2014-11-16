# Start the clock!
ptm <- proc.time()

# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)

# FUNCTION DEFINITIONS #

# Make Corpus, Transform, Make Trigram TDM
makeTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<- TermDocumentMatrix(corpus)
#tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}

## DATA MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#

fileMunge<- function(x) {
text<-readLines(x)
totalLines=length(text)
chunkSize=20000
chunks=totalLines/chunkSize
remainder = chunks %% 1
wholeChunks = chunks-remainder
# initialize list
output=list()
# break file into chunks 
i=1
line=1
while (i<=wholeChunks){
end=line+chunkSize-1
output[[i]]<-text[line:end]
line=end+1
i=i+1
}
output[[i]]<-text[line:totalLines]
# Text Transformations to remove odd characters #
output=lapply(output,FUN=iconv, to='ASCII', sub=' ')
output=lapply(output,FUN= function(x) gsub("'{2}", " ",x))
output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
}

test<-fileMunge("testData1.txt")

testTDM <- makeTDM(test)

m <- as.matrix(testTDM)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, 10)
tTop=v[1:10]
# names(tTop)

tDF=data.frame(Words=names(tTop), Freq=tTop)

# par(mfcol=c(3,1))
barplot(bTop, col ="firebrick1", breaks=seq(0,max(tTop),by=1), main="Top 10 Words: Blogs Corpus", xlab = "Frequency", cex.lab = 1.3,xlim=c(0,25), horiz=TRUE, las=1)

barplot(tTop, col ="chocolate2", breaks=seq(0,max(tTop),by=1), main="Top 10 Words: News Corpus", xlab = "Frequency", cex.lab = 1.3,xlim=c(0,25), horiz=TRUE, las=1)

barplot(nTop, col ="cadetblue3", breaks=seq(0,max(tTop),by=1), main="Top 10 Words: Twitter Corpus", xlab = "Frequency", cex.lab = 1.3,xlim=c(0,25), horiz=TRUE, las=1)
