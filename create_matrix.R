create_matrix <- function(trainingColumn, language="en", minDocFreq=1, minWordFreq=1, minWordLength=3, removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, selectFreqTerms=0, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {	
	stem_words <- function(x, language) {
		tokens <- strsplit(x," ")[[1]]
		tokens <- substr(tokens,1,255)
		stemmed <- wordStem(tokens,language=language)
		return(iconv(paste(stemmed,collapse=" "),to="UTF8",sub="byte"))
	}
	
	select_wordFreq <- function(x, language, freq, control) {
		corpus <- Corpus(VectorSource(x),readerControl=list(language=language))
		matrix <- DocumentTermMatrix(corpus,control=control)
		freqWords <- findFreqTerms(matrix,freq)
		if (length(freqWords) == 0) return("")
		return(iconv(paste(freqWords,collapse=" "),to="UTF8",sub="byte"))
	}
	
	select_topFreq <- function(x, language, cutoff, control) {
		corpus <- Corpus(VectorSource(x),readerControl=list(language=language))
		matrix <- as.matrix(DocumentTermMatrix(corpus,control=control))
		termCol <- cbind(colnames(matrix),matrix[1,])
		wordDist <- sort(termCol[,2],decreasing=TRUE)
		topWords <- rownames(as.matrix(wordDist))[0:cutoff]
		if (length(topWords) == 0) return("")
		return(iconv(paste(topWords[!is.na(topWords)],collapse=" "),to="UTF8",sub="byte"))
	}
	
	control <- list(weighting=weighting,language=language,tolower=toLower,stopwords=removeStopwords,removePunctuation=removePunctuation,removeNumbers=removeNumbers, stripWhitespace=TRUE, minWordLength=minWordLength , minDocFreq=minDocFreq)
	if (stemWords == TRUE) trainingColumn <- as.vector(sapply(trainingColumn,stem_words,language=language),mode="character")
	if (selectFreqTerms > 0) trainingColumn <- as.vector(sapply(trainingColumn,select_topFreq,language=language,cutoff=selectFreqTerms,control=control),mode="character")
	if (minWordFreq > 1) trainingColumn <- as.vector(sapply(trainingColumn,select_wordFreq,language=language,freq=minWordFreq,control=control),mode="character")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms) # Advisable value for removeSparseTerms: 0.9998
	
	gc()
	return(matrix)
}