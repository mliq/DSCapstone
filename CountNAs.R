
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
