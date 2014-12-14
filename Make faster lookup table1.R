#Make faster lookup table.R

x=readRDS("t.no4counts.RDS")

#First create Trigrams so rest will work?

	# get gram, separate words
		gram=x[1,]$grams
		words<-unlist(strsplit(gram,"\\s+"))

	# Isolate FIRST two words of the sentence
		history=words[(length(words)-2):(length(words)-1)]
		prediction=words[length(words)]
		history=paste(as.character(history),collapse=' ')
		histstring=str_replace_all(history, "[[:punct:]]", "?")

	# find all rows with that history, only keep the top one.
		matches=data.table(x[grep(paste0("^",histstring," "),x$grams),][order(-counts)])

		# Isolate prediction words
		pred=matches[1]$grams
		pred=unlist(strsplit(pred,"\\s+"))
		pred=pred[length(pred)]

		# Set in data.table
		Trigrams=data.table(hist=history,pred=pred)
		setkey(Trigrams,hist)

lapply(2:nrow(x),function(y){

	# get gram, separate words
		gram=x[y,]$grams
		words<-unlist(strsplit(gram,"\\s+"))
		
	# Isolate FIRST two words of the sentence
		history=words[(length(words)-2):(length(words)-1)]
		prediction=words[length(words)]
		history=paste(as.character(history),collapse=' ')
	
	# Replace regex characters with wildcard so search works.
		histstring=str_replace_all(history, "[[:punct:]]", "?")

	# Skip the rest if it's already been done
		if(is.na(Trigrams[history])[2]==TRUE){

			# find all rows with that history, only keep the top one.
			matches=data.table(x[grep(paste0("^",histstring," "),x$grams),][order(-counts)])
	
			if(nrow(matches)>0){
				# Isolate prediction words
				pred=matches[1]$grams
				pred=unlist(strsplit(pred,"\\s+"))
				pred=pred[length(pred)]

				# Set in data.table
				NewValue=data.table(hist=history,pred=pred)
				setkey(NewValue,hist)

				#Add to Trigrams list after it already exists.
				if(nrow(Trigrams)>=1){
					Trigrams<<-rbindlist(list(NewValue,Trigrams), use.names=TRUE)
					setkey(Trigrams,hist)
				}
			}
		}
})