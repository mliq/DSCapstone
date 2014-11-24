# process training set #

# Start the clock!
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
train=readRDS("t.train.RDS")
library(tm)
library(RWeka)

# FUNCTION DEFINITIONS #

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
# corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
# corpus<- tm_map(corpus,removePunctuation)
# corpus<- tm_map(corpus,removeNumbers)
return(corpus)
}

process<- function(x) {
# Text Transformations to remove odd characters #
# replace APOSTROPHES OF 2 OR MORE with space - WHY??? that never happens..
	# output=lapply(output,FUN= function(x) gsub("'{2}"rr, " ",x))
# Replace numbers with spaces... not sure why to do that yet either.
	# output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
# Erase commas.
x=gsub(",?", "", x)
# Erase ellipsis
x=gsub("\\.{3,}", "", x)
# Erase colon
x=gsub("\\:", "", x)
# Merge on contractions (apostrophe):
x=gsub("\\'", "", x)
# Erase |:
x=gsub("\\|", "", x)
# Erase {}:
x=gsub("\\{", "", x)
x=gsub("\\}", "", x)
##### SENTENCE SPLITTING AND CLEANUP
# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
# ok here is where you change structure fundamentally... 
# Faster if I unlist once? no i guess it keeps getting relisted.
x<-strsplit(unlist(x),"[\\.]{1}")
x<-strsplit(unlist(x),"\\?+")
x<-strsplit(unlist(x),"\\!+")
x<-strsplit(unlist(x),"\\-+")
# Split also on parentheses
x<-strsplit(unlist(x),"\\(+")
x<-strsplit(unlist(x),"\\)+")
# split also on quotation marks
x<-strsplit(unlist(x),"\\\"")
# remove spaces at start and end of sentences:
# HERE is where the problem begins. why?
x<-gsub("^\\s+", "", unlist(x))
x<-gsub("\\s+$", "", unlist(x))
# Replace ~ and any whitespace around with just one space
x<-gsub("\\s*~\\s*", " ", unlist(x))
# Replace forward slash with space
x<-gsub("\\/", " ", unlist(x))
# Replace + signs with space
x<-gsub("\\+", " ", unlist(x))
# it s a 
x<-gsub("it s ", "its ", unlist(x))
# 'i m not'
x<-gsub("i m not", "im not", unlist(x))
# 'i didn t'
x<-gsub("i didn t", "i didnt", unlist(x))
# 'i don t'
x<-gsub("i don t", "i dont", unlist(x))
# ' i m '
x<-gsub(" i m ", " im ", unlist(x))

# Eliminate empty and single letter values (more?)
x=x[which(nchar(x)!=1)]
x=x[which(nchar(x)!=0)]
}

# Tokenizer functions
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# Corpus, transformations, and TDM Creation
#=============================================#
train=process(train)

corpus<-makeCorpus(train)

Ttdm<- TermDocumentMatrix(corpus, control = list(tokenize = TgramTokenizer))
gc()

rm(corpus)

#########################
#BUILD PREDICTION TABLES#
#########################

library(slam)
library(data.table)

counts=row_sums(Ttdm)
rm(Ttdm)
gc()
Tfreq<-data.table(grams=names(counts), counts=counts)
setkey(Tfreq,grams)
gc()

rm(counts)
#rm(train)
# Stop the clock
proc.time() - ptm # 10000 news lines: 74 secs 1.23 minutes

