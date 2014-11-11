setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US/twit10")
library(tm)
twit<-list()
# First load all data into list #
for (i in 0:118) {
i4<-formatC(i,width=4,flag=0)
fileName=paste0("twitter",i4)
#assign(paste0("twit",i),readLines(fileName))
twit[i+1]<-list(readLines(fileName))
}

twit=lapply(twit,FUN=iconv, to='ASCII', sub=' ')
twit=lapply(twit,FUN= function(x) gsub("'{2}", " ",x))
twit=lapply(twit,FUN= function(x) gsub("[0-9]", " ",x))

#Make TDM one at a time function:
makeTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,removeNumbers)
tdm<-TermDocumentMatrix(corpus)
tdm<-removeSparseTerms(tdm,0.50)
return(tdm)}

# ok this works.. this is progress..  Can this be lapplied?
# I don't think so if I can't keep TDMs in a list...
#wow, this went damn fast... but it's only returning 1 tdm.
test<-twit[1:3]
testTDM<-makeTDM(test)
#holy shit this works!
# # TESTing
#findAssocs(testTDM,"beer",.98)
# findFreqTerms(testTDM,300)

#OK now for real:

myTDM<-makeTDM(twit)

#Yes it worked! In just 5 minutes or so!
#<<TermDocumentMatrix (terms: 11235, documents: 119)>>
# Non-/sparse entries: 1097831/239134
# Sparsity           : 18%
# Maximal term length: 22
# Weighting          : term frequency (tf)

# findAssocs(myTDM,"beer",.55)
# findAssocs(myTDM,"case",.40)
# But actually.. now that my documents are arbitrary, this findAssocs is totally meaningless. Time for the trigram model.



#######################

# second do transformations on all #
myTDM=list()
for(i in 1:length(test)){
tdm=makeTDM(test[i])
return(tdm)
}
myTDM

#test<-lapply(test,FUN=makeTDM)
#test<-sapply(test,FUN=makeTDM)

testTDM=list()
testTDM=lapply(test,FUN=makeTDM)





#This will have the same problem.
twit=lapply(twit,FUN= function(x) Corpus(VectorSource(x)))

twit=lapply(twit,FUN=)

makeCorpus <- function(x) corpus <- {
	corpus<-Corpus(VectorSource(x))
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	corpus <- tm_map(corpus, stemDocument)
	corpus<- tm_map(corpus,removePunctuation)
	corpus<- tm_map(corpus,removeNumbers)
}

makeCorpus <- function(x) corpus <- {
	corpus<-Corpus(VectorSource(x))
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	corpus <- tm_map(corpus, stemDocument)
	corpus<- tm_map(corpus,removePunctuation)
	corpus<- tm_map(corpus,removeNumbers)
}

twitCorp=lapply(twit,FUN=makeCorpus)

twit3<-makeCorpus(twit2)

tdm<-TermDocumentMatrix(twit3)
tdm<-removeSparseTerms(tdm,0.50)


for (i in 1:119) {
twit[i]<-iconv(twit[i], to='ASCII', sub=' ')
}
# BUT lapply should be faster.
twit=lapply(twit,FUN=iconv, to='ASCII', sub=' ')

# Replace numbers and ''
twit <- gsub("'{2}", " ", twit)
twit <- gsub("[0-9]", " ", twit)

## PROCESS THE FILES FIRST.. ##
for (i in 0:1) {
i4<-formatC(i,width=4,flag=0)
fileName=paste0("twitter",i4)
temp<-dget(fileName)

}
##
dput(twit, file="twit.dput")
twit2<-dget("twit.dput")
twit2 <- gsub("'{2}", " ", twit2)
twit2 <- gsub("[0-9]", " ", twit2)
twit3<-makeCorpus(twit2)

tdm<-TermDocumentMatrix(twit3)
tdm<-removeSparseTerms(tdm,0.50)

#OK let's try to re-implement this using removeSparseTerms each 10 docs. ALso, backup corpus/tdm somehow??
#backup
dput(tdm, file="tdm1-10")
#retreive
dget("tdm1-10")

twit<-list()
for (i in 0:10) {
i4<-formatC(i,width=4,flag=0)
fileName=paste0("twitter",i4)
#assign(paste0("twit",i),readLines(fileName))
twit[i]<-list(readLines(fileName))
}


#------#
findFreqTerms(x = myTDM, lowfreq=1, highfreq=Inf)


twit <- VCorpus(DirSource("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US/twit10", encoding = "UTF-8"),readerControl = list(language = "en"))
	corpus<-twit
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	corpus <- tm_map(corpus, stemDocument)
	corpus<- tm_map(corpus,removePunctuation)
	corpus<- tm_map(corpus,removeNumbers)

tdm<-TermDocumentMatrix(cleanData3)

cleanData<-iconv(corpus, to='ASCII', sub=' ')
# Replace numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)

#-----#
print(twit)
inspect(twit[1:2])

readPlain(twit,english,twitter)



txt <- system.file("texts","C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US",package="tm")


## CORPUS MAKE FUNCTION DEFINE ##
library(tm)
makeCorpus <- function(x) corpus <- {
	corpus<-Corpus(VectorSource(x))
	corpus <- tm_map(corpus, stripWhitespace)
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	corpus <- tm_map(corpus, stemDocument)
	corpus<- tm_map(corpus,removePunctuation)
	corpus<- tm_map(corpus,removeNumbers)
}
# TrigramTokenizer function define
# Build TDM with tri-grams.
library(rJava) # Is this really needed?
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

## CORPUS MUNGING ##

# 1. Corpus, transformations, and TDM Creation
#=============================================#
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
fileName="en_US.twitter.txt"
lineNews <- readLines(fileName, n=20000)

# First I use a Unix Shell command to split the text file into 20,000 lines each chunks:
# split --numeric-suffixes -a 4 --lines=20000 en_US.twitter.txt twitter

# How about a list?
twit<-list()
for (i in 1:118) {
i4<-formatC(i,width=4,flag=0)
fileName=paste0("twitter",i4)
#assign(paste0("twit",i),readLines(fileName))
twit[i]<-list(readLines(fileName))
}

# Make large TDM by doing each chunk, then tm_combine the TDMs to prevent crashing from large Corpus files

# Make First TDMs
lineData=twit[[1]]
## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.
cleanData<-iconv(lineData, to='ASCII', sub=' ')
# Replace numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)
## MAKE CORPUS ##
corpus<-makeCorpus(cleanData3)

myTDM <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

# OK current TDM has 106013 terms.
# After iterating 2:3 it has the same...
# ok now has 312208

for (i in 1:118){
lineData=twit[[i]]
## REMOVAL OF STRANGE CHARACTERS##
# Replace unicode characters with spaces.

cleanData<-iconv(lineData, to='ASCII', sub=' ')
# Replace numbers and ''
cleanData2 <- gsub("'{2}", " ", cleanData)
cleanData3 <- gsub("[0-9]", " ", cleanData2)
## MAKE CORPUS ##
corpus<-makeCorpus(cleanData3)

newTDM <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
myTDM <- c(myTDM,newTDM)
}

# OK i canceled after an hour and now we have:
# 9270280 Terms, 2020000 Docs.

## Export? ##
m <- inspect(myTDM)
DF <- as.data.frame(m, stringsAsFactors = FALSE)
write.table(DF)

# 2. Isolate bigrams and unigrams within trigrams 
#=============================================#

# Get total frequency in corpus of each trigram
library(slam)
gramCount<-as.matrix(row_sums(myTDM))

# Create dataframe frequency table
freqTable <- data.frame(gram=dimnames(gramCount)[[1]],count=gramCount,stringsAsFactors=FALSE)
# Split corpus trigrams up to words
words <- strsplit(freqTable$gram," ")

# Set first two words as an attribute, the trigram prediction query pair of words
freqTable$triquery <- sapply(words,FUN=function(x) paste(x[1],x[2]))

# Set each word of trigram as an attribute for future use
freqTable$one <- sapply(words,FUN=function(x) paste(x[1]))
freqTable$two <- sapply(words,FUN=function(x) paste(x[2]))
freqTable$three <- sapply(words,FUN=function(x) paste(x[3]))

# 3. INPUT MUNGING
#=============================================#
## INPUT MUNGING ##
# i. Take an input:
input<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

# ii. Perform Transformations.
input<-makeCorpus(input)
input<-as.character(input[[1]][1])

# iii. Reduce to last two words
input<-unlist(strsplit(input,"\\s+"))
two<-length(input)
one<-two-1
# iv. set querying bigrams and unigrams we will search for in trigrams and bigrams respectively
bigram<-paste(input[one],input[two])
unigram<-paste(input[two])

# 4. FIND PREDICTION MATCHES
#=============================================#

#TRIGRAMS:

# Find trigrams where first two words match and put in matches list
trimatches <- freqTable[freqTable$triquery == bigram,]
bimatch1 <- freqTable[freqTable$one == unigram,]
bimatch2 <- freqTable[freqTable$two == unigram,]

# Put these results in a frequency table and rank them as such.
matches<-c(trimatches$three,bimatch1$two,bimatch2$one)
matchCorpus<-makeCorpus(matches) # (Corpus(VectorSource(matches)) caused very similar terms to be considered separately like singular and plural)
matchTDM<-TermDocumentMatrix(matchCorpus)

# Get total frequency in prediction corpus of each prediction
predCount<-rowSums(as.matrix(matchTDM))

# Create dataframe frequency table
predFreq <- data.frame(gram=names(predCount),count=predCount,stringsAsFactors=FALSE)
predFreq<-predFreq[order(-predFreq$count),]

predFreq
gc()
