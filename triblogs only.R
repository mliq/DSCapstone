# Start the clock!
ptm <- proc.time()

# SETUP #
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(xtable)
library(RWeka)

# FUNCTION DEFINITIONS #
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# Make Corpus, Transform, Make Trigram TDM
makeTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<- TermDocumentMatrix(corpus, control = list(tokenize = TgramTokenizer))
#tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}

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
## DATA MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#
b.dat<-fileMunge("en_US.blogs.txt")
b.tdm <- makeTDM(b.dat)
rm(blog)
gc()

# Stop the clock
proc.time() - ptm # 1522.53
# saved here as triblogs only.TDMloaded
library(slam)
counts=row_sums(b.tdm)
triblog<-data.frame(grams=names(counts), counts=counts)
# now, as a data.frame this takes 2.8 GB, not cool
# counts has all the data and it's only 1.3 GB
# so we could potentially work with that... if we find a way to query...
# the exact query can go like this:
which(names(counts)=="a a a")

but how to regex like *

grep("a a ",names(counts))
# I think it's working...
rows<-grep("a a ",names(counts))
results<-data.frame(names=names(counts[rows]),counts=counts[rows])
# hell yeah...

predict<-function(x){
rows<-grep(x,names(counts))
results<-data.frame(names=names(counts[rows]),counts=counts[rows])
results<-results[order(-results$counts),]
}

results<-predict(" at the ") # must remember to add spaces
#hmmm...
#not sure why that did not work... ah no space at the end probably.
# well one or the other....
results<-predict("at the ")
results<-predict("at the beach") #192
results<-predict("at the mall") # 65
results<-predict("at the movies") #16
results<-predict("at the movi")  #18

results<-predict("on my ")  #



#b.tdm2 <- removeSparseTerms(b.tdm, 0.5)

# Definitely need some removeSparse or else we crash.
# or cloud computing.
# Maybe just as.matrix is the issue though.

# top N words
#m <- as.matrix(b.tdm)
#v <- sort(rowSums(m), decreasing=TRUE)


#bTop=v[10:1]

# saveRDS(bTop,"b3Top")

# options("max.print"=1000000)
