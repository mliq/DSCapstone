# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(tm)
library(RWeka)

# Load the large twitter data.table trigram
library(data.table)

#Shrink procedure:
	#Tfreq=readRDS("b.Tfreq4.RDS")
	# For t: counts>2 = 396763/5266289 0.07534014939172536866%
		# counts>=2 791025/5266289 .15% OK
	# b: counts>=2 1056188/6717146=0.15723761252174658702
	#Tfreq=Tfreq[counts>=2,] # 791025/5266289 .15% OK

	#saveRDS(Tfreq,file="b.Tfreq4.shrink.RDS")

Tfreq=readRDS("b4.t6.n4.Tfreq.RDS")
	
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
	test=scan("Quiz2.txt", what="character",n=1,skip=x)

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


############################
countNAs=function(x){
	
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
	NACount=0
	total=length(words)-2
	# how many trigrams will there be? 
	# in a 4 word sentence, 2 so length-2.
	# therefore, need exception not to run this when length is less than 3 words.
	if(length(words)>=3){
		count=lapply(1:total,FUN=function(x){
			# loop through sentence making bigram and answer, 
			bigram=paste(words[x], words[x+1])
			answer=paste(words[x+2])
			# then check answer against predicted answer.
			# Get answer
			Xpred=data.table(Tfreq[grep(paste0("^",bigram," "),Tfreq$grams),][order(-counts)])	
			# isolate the answer from prediction table.
			# why not test for nrows and skip this?
			Xpred=unlist(strsplit(Xpred[1]$grams,"\\s+"))
			Xpred=Xpred[length(Xpred)]
			# Test for na
			if(is.na(Xpred)){
				NACount=NACount+1
			}
		})
	}
	return(count)
}

Count=lapply(0:9,countNAs)
sum(unlist(Count))

sum(as.numeric(summary(Count)[1:length(Count)]))

# b.Tfreq4.shrink.RDS  20 / ?
# t.Tfreq6.shrink.RDS 23 / 141 = 16%
----
Accuracy Measures
# 0.1593463 for t.Tfreq6.shrink.RDS : ok no drop that's cool...
# 0.1545497 for b.Tfreq4.shrink.RDS : some drop. darn.

# 0.063259 for t.Tfreq4.RDS
# 0.1593463 for t.Tfreq6.RDS

# 0.1361028 for n.Tfreq4.RDS
# 0.1737417 for b.Tfreq4.RDS

# history=words[(length(words)-1):length(words)]
# nMin1=words[length(words)]
# history=paste(as.character(history),collapse=' ')


# # Make prediction list of matches:
# Tpred=data.table(Tfreq[grep(paste0("^",history," "),Tfreq$grams),][order(-counts)])

# # Print out top 5 possibilities:
# print(Tpred[1:25])
# Tpred<<-Tpred
# }

# # Find function
# find=function(x){
# Tpred[grep(x,Tpred$grams),]
# }

# Calculate Probabilities?
# Edit trigrams to just the prediction and the count and the probabilities
# Tpred=Tpred[,{s=strsplit(grams," ");list(prediction=unlist(s)[c(FALSE,FALSE,TRUE)],counts=counts,probability=probability)}]

# Stop the clock
proc.time() - ptm # 1e6 twit: 35.15 secs

# ok so now all is dandy except my predictions blow.
# study model again.
####DATA.TABLE REFERENCE####
# Subset listed predictions:
# Tpred[[1]][Tpred[[1]]$counts>5]
# how about search:
# test=Tfreq[counts>2]
# Fuzzy / Inexact Matches:
# test[grep("a lot",test$grams)]
# Only the first two words of trigram:
# test[grep("^mani time",test$grams)]
# grepl to get line numbers?
# example(data.table)
# Join two tables and match the keys, summing the values:
# DT=merge(DT1,DT2,all=TRUE)
# DT[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = names]
# DT[,c("counts.x","counts.y") := NULL]
# help(":=")
# rename a column quickly:
# setnames(Ufreq,"old","new")
####DATA.TABLE REFERENCE####