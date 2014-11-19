# Start the clock!
ptm <- proc.time()
##########################

# SETUP #
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

# Modify fileMunge to leave sentence separators intact and only take 10 lines for now.

fileMunge<- function(x) {
text<-readLines(x)
totalLines=length(text)
chunkSize=20000
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
# Text Transformations to remove odd characters #
# CONVERT TO ASCII
output=lapply(output,FUN=iconv, to='ASCII', sub=' ')
# replace APOSTROPHES OF 2 OR MORE with space - WHY??? that never happens..
	# output=lapply(output,FUN= function(x) gsub("'{2}", " ",x))
# Replace numbers with spaces... not sure why to do that yet either.
	# output=lapply(output,FUN= function(x) gsub("[0-9]", " ",x))
}

twit<-fileMunge("twit100.txt")

##########################
# Stop the clock
proc.time() - ptm # 1522.53
