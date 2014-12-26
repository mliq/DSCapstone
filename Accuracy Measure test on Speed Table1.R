# PRE-PROCESSING #
	gc()
	setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
	library(tm)
	library(RWeka)
	library(data.table)
	library(SnowballC)
	
	# Trigrams
	tfreq=readRDS("speed.test.RDS")
	#bfreq=readRDS("b.no5counts.RDS")
	#nfreq=readRDS("n.no5counts.RDS")
	#afreq=readRDS("ALL.no5counts.RDS")
    
  ## FUNCTION DEFINITIONS ##

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
		x<-strsplit(unlist(x),"\\!+") # Error: non-character argument?
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

	classify=function(y,z){
	  total=length(z)-2
	  correct=0
	  lapply(1:total,FUN=function(x){
			# loop through sentence making bigram and answer, 
			bigram=paste(z[x], z[x+1])
			answer=paste(z[x+2])
			# then check answer against predicted answer.
			# Get answer
			Xpred=y[bigram]$pred
			# Test equality of prediction to actual and counter for the accuracy measure
			if(!is.na(Xpred)){
			  if(Xpred==answer){correct=correct+1}	
			  correct<<-correct
			}
		})
		accuracy = correct/total
		return(accuracy)
	}

	#### INPUT MUNGING ####
  getPred=function(x){
		
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
		
		# Classify text (if 3 words or more)
		
		if(length(words)>=3){
		 # b.acc=classify(bfreq,words)
		  t.acc=classify(tfreq,words)
		 # n.acc=classify(nfreq,words)
		 # a.acc=classify(afreq,words)
		}  

		# Select frequency table based on classification results.
		# if(b.acc>t.acc && b.acc>n.acc && b.acc>a.acc){
		# 	acc=b.acc
		# } else if(t.acc>b.acc && t.acc>n.acc && t.acc>a.acc){
		# 	acc=t.acc
		# } else if(n.acc>b.acc && n.acc>t.acc && n.acc>a.acc){
		# 	acc=n.acc
		# } else if(a.acc>b.acc && a.acc>t.acc && a.acc>n.acc){
		# 	acc=a.acc
		# } else {
		# 	acc=a.acc
		# }
		acc=t.acc 
		return(acc)
  }
  #####################################################
  
  # OUTPUT ACCURACY #
  
  results=unlist(lapply(0:9,getPred))
	mean(results)	