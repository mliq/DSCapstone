##########################

# SETUP #
# setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
#options("max.print"=1000000)

# Modify fileMunge to leave sentence separators intact and only take 10 lines for now.

# Start the clock!
ptm <- proc.time()
fileMunge<- function(x) {
text<-readLines(x)
totalLines=length(text)
chunkSize=10000
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
# CONVERT TO ASCII
output=lapply(output,FUN=iconv, to='ASCII', sub=' ')
}

twit<-fileMunge("en_US.twitter.txt")

# Process each element of list into Sentences

process<- function(output) {
# Text Transformations to remove odd characters #
# replace APOSTROPHES OF 2 OR MORE with space - WHY??? that never happens..
	# output=lapply(output,FUN= function(x) gsub("'{2}", " ",x))
# Replace numbers with spaces... not sure why to do that yet either.
	# output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
# Erase commas.
output=lapply(output,FUN=function(x) gsub(",?", "", x))
# Erase ellipsis
output=lapply(output,FUN=function(x) gsub("\\.{3,}", "", x))
# Erase colon
output=lapply(output,FUN=function(x) gsub("\\:", "", x))
##### SENTENCE SPLITTING AND CLEANUP
# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
output<-strsplit(output[[1]],"[\\.]{1}")
output<-strsplit(unlist(output),"\\?+")
output<-strsplit(unlist(output),"\\!+")
output<-strsplit(unlist(output),"\\-+")
# Split also on parentheses
output<-strsplit(unlist(output),"\\(+")
output<-strsplit(unlist(output),"\\)+")
# split also on quotation marks
output<-strsplit(unlist(output),"\\\"")
# remove spaces at start and end of sentences:
output<-lapply(output,FUN=function(x) gsub("^\\s+", "", x))
output<-lapply(output,FUN=function(x) gsub("\\s+$", "", x))
# Replace ~ and any whitespace around with just one space
output<-lapply(output,FUN=function(x) gsub("\\s*~\\s*", " ", x))
# Replace forward slash with space
output<-lapply(output,FUN=function(x) gsub("\\/", " ", x))
# Replace + signs with space
output<-lapply(output,FUN=function(x) gsub("\\+", " ", x))
# Eliminate empty and single letter values (more?)
output[which(nchar(unlist(unlist(output)))==1)]=NULL
output[which(nchar(unlist(unlist(output)))==0)]=NULL
output=unlist(output)
}

twit<-lapply(twit,FUN=function(x){lapply(x,process)})
# OK i think that worked, taking 514MB.
# so.. twit2[[x]][[y]][z] where x=chunk, y=line, z=sentence

# Stop the clock
proc.time() - ptm
########################## 
# END SENTENCE CREATION  #
########################## 
########################## 
# BUILD ASSOCIATED WORDS DATABASE (ALL FOUND IN SAME SENTENCE)  #
##########################
# tdm: lowercase, stem, TDM

library(tm)
library(RWeka)
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

makeTriTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument)
tdm<- TermDocumentMatrix(corpus, control = list(tokenize = TgramTokenizer))
#tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}


########################## 
##########################
# FOLLOWING MUST GO ONE CHUNK AT A TIME #
########################## 
########################## 

# database creation (CHANGE FILENAME!)
library("filehash")
filehashOption("DB1")
dbCreate("t.tri")
db <- dbInit("t.tri", type="DB1")

# Trigram to DB function
library(slam)

# Start the clock!
ptm <- proc.time()
trigramToDB(twit[[1]][1:3191])
# Stop the clock
proc.time() - ptm
# 500 lines takes: 94 seconds.
# SO, 2.36e6, whole twitter will take: 4720*94=443680 seconds, 7394.66666666666666666667 min, 123 hours.
# So why was I able to do this before...

# Walk through this with a small set first.
x=twit[[1]][1:500]

trigramToDB<-function(x){
tdm <- makeTriTDM(x)
# create vector of total frequencies
counts=row_sums(tdm)
rm(tdm)
gc()
# put to database
lapply(1:length(counts),FUN=function(x){
key=names(counts[x])
c=counts[x]
if(dbExists(db,key)==TRUE)
{
existing=db[[key]]
db[[key]]=(existing+c)
}
else
{
db[[key]]=c
}
})
}