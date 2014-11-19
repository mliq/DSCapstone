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
# Erase commas.
output=lapply(output,FUN=function(x) gsub(",?", "", x))
# Erase ellipsis
output=lapply(output,FUN=function(x) gsub("\\.{3,}", "", x))
# Erase colon
output=lapply(output,FUN=function(x) gsub("\\:", "", x))
}

twit<-fileMunge("twit100.txt")

# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
twit2<-strsplit(twit[[1]],"[\\.]{1}")
twit3<-strsplit(unlist(twit2),"\\?+")
twit4<-strsplit(unlist(twit3),"\\!+")
twit5<-strsplit(unlist(twit4),"\\-+")
# Split also on parentheses
twit6<-strsplit(unlist(twit5),"\\(+")
twit7<-strsplit(unlist(twit6),"\\)+")
# split also on quotation marks
twit8<-strsplit(unlist(twit7),"\\\"")
# remove spaces at start and end of sentences:
twit9<-lapply(twit8,FUN=function(x) gsub("^\\s+", "", x))
twit10<-lapply(twit9,FUN=function(x) gsub("\\s+$", "", x))
# Replace ~ and any whitespace around with just one space
twit11<-lapply(twit10,FUN=function(x) gsub("\\s*~\\s*", " ", x))
# Eliminate empty and single letter values (more?)
twit12[which(nchar(twit12)==1)]=NULL
twit12[which(nchar(twit12)==0)]=NULL

##########################
# Stop the clock
proc.time() - ptm # 1522.53


##########################
# REFERENCE:
# grep to find a quotation mark: grep("\\\"",twit4)
# all this time my problem has been the original reference duhr! (list, not char)
# If you want to just get the set of values that are not "": 
# twit12[twit12 != ""] 
# which(nchar(twit12)==1)
# which(nchar(twit12)==0)

# this did not work: 
# twit12<-twit10[unlist(twit11) != ""] 