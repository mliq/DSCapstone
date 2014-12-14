# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(RWeka)

# Load the large twitter data.table trigram
library(data.table)

Tfreq=readRDS("t.1of3.Tfreq.RDS")
	
## FUNCTIONS ##

# Make Corpus and do transformations
makeCorpus<- function(x) {
corpus<-Corpus(VectorSource(x))
# corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus<- tm_map(corpus,removePunctuation)
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
#####################################################
#Predictions:#
# Start the clock!
ptm <- proc.time()

#### INPUT MUNGING ####
getPred=function(x){
	#TEST:
	#x=0
	# Take an input:
	test=scan("Quiz3.txt", what="character",n=1,skip=x)

	# transform as training set was (lowercase, stem, strip punctuation etc.)
	test=iconv(test, to='ASCII', sub=' ')
	test=process(test)
	test=paste0(test, collapse=" ")
	corpus<-makeCorpus(test)
	corpus=as.character(corpus[[1]][1])

	# Split by words:
	words<-unlist(strsplit(corpus,"\\s+"))

	# Isolate last two words of the sentence
	# Loop here to make set of trigrams.
	correct=0
	total=length(words)-2
	# how many trigrams will there be? 
	# in a 4 word sentence, 2 so length-2.
	# therefore, need exception not to run this when length is less than 3 words.
	if(length(words)>=3){
		lapply(1:total,FUN=function(x){
			# loop through sentence making bigram and answer, 
			bigram=paste(words[x], words[x+1])
			answer=paste(words[x+2])
			# then check answer against predicted answer.
			# Get answer
			Xpred=data.table(Tfreq[grep(paste0("^",bigram," "),Tfreq$grams),][order(-counts)])	
			# isolate the answer from prediction table.
			Xpred=unlist(strsplit(Xpred[1]$grams,"\\s+"))
			Xpred=Xpred[length(Xpred)]
			# Test equality of prediction to actual and counter for the accuracy measure
			if(!is.na(Xpred)){
				if(Xpred==answer){correct=correct+1}	
				correct<<-correct
			} 
		})
	}
	accuracy = correct/total
	# paste("Correct: ", correct, "total: ", total, "Accuracy: ", accuracy)
	return(accuracy)
}

check<-function(x){
	bigram=x
	Xpred=data.table(Tfreq[grep(paste0("^",bigram," "),Tfreq$grams),][order(-counts)])
	Xpred=unlist(strsplit(Xpred[1]$grams,"\\s+"))
	Xpred=Xpred[length(Xpred)]
	paste(bigram, Xpred)
}

# Do get pred over each line of a document and print all results

# Quiz 3 Test
results=unlist(lapply(0:9,getPred))
mean(results)