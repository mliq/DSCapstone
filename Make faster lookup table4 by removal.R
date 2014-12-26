#Make faster lookup table.R
# Start the clock!
ptm <- proc.time()

library("stringr")
library("data.table")

x=readRDS("t.no4counts.RDS")

# add row number column
x[,Rnum:=seq_len(nrow(x))]

for(i in 1:nrow(x)){

	# get gram, separate words
		words<-unlist(strsplit(x[i]$grams,"\\s+"))

	# Isolate FIRST two words of the sentence
		history=words[(length(words)-2):(length(words)-1)]
		prediction=words[length(words)]
		history=paste(as.character(history),collapse=' ')
		histstring=str_replace_all(history, "[[:punct:]]", "?")

	# find all rows with that history, only keep the top one.
		xrows=x[grep(paste0("^",histstring," "),x$grams)]$Rnum
		z=x[xrows][order(-counts)][1]
		xrows=xrows[xrows!=z$Rnum]

	# Erase all rows but top
		x=x[-xrows]
}

# Stop the clock
proc.time() - ptm

#Remember to erase any NA row at the end...