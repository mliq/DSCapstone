# Start the clock!
ptm <- proc.time()
##########################

# SETUP #
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
#options("max.print"=1000000)

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
# CONVERT TO ASCII
output=lapply(output,FUN=iconv, to='ASCII', sub=' ')
}

twit<-fileMunge("en_US.twitter.txt")

# Process each element of list into Sentences

process<- function(output) {
# Text Transformations to remove odd characters #
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
##### SENTENCE SPLITTING AND CLEANUP
# Split into sentences only on single periods or any amount of question marks or exclamation marks and -
output<-strsplit(output[[1]],"[\\.]{1}")
output<-strsplit(unlist(output),"\\?+")
output<-strsplit(unlist(output),"\\!+")
output<-strsplit(unlist(output),"\\-+")
# Split also on parentheses
output<-strsplit(unlist(output),"\\(+")
output<-strsplit(unlist(output),"\\)+")
# split also on quotation marks
output<-strsplit(unlist(output),"\\\"")
# remove spaces at start and end of sentences:
output<-lapply(output,FUN=function(x) gsub("^\\s+", "", x))
output<-lapply(output,FUN=function(x) gsub("\\s+$", "", x))
# Replace ~ and any whitespace around with just one space
output<-lapply(output,FUN=function(x) gsub("\\s*~\\s*", " ", x))
# Replace forward slash with space
output<-lapply(output,FUN=function(x) gsub("\\/", " ", x))
# Eliminate empty and single letter values (more?)
output[which(nchar(unlist(unlist(output)))==1)]=NULL
output[which(nchar(unlist(unlist(output)))==0)]=NULL
output=unlist(output)
}

twit<-lapply(twit,FUN=function(x){lapply(x,process)})
# OK i think that worked, taking 514MB.
# so.. twit2[[x]][[y]][z] where x=chunk, y=line, z=sentence
# HERE is where I must accont for this extra layer? Yet, if I keep passing in twit[[1]] why do I??

# Stop the clock
proc.time() - ptm
########################## 
# END SENTENCE CREATION  #
########################## 
########################## 
# BUILD ASSOCIATED WORDS DATABASE (ALL FOUND IN SAME SENTENCE)  #
##########################
# tdm: lowercase, stem, TDM

library(tm)
makeTDM <- function(x) {
corpus<-Corpus(VectorSource(x))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument)
tdm<- TermDocumentMatrix(corpus)
#tdm<-removeSparseTerms(tdm,0.97)
return(tdm)}

########################## 
##########################
# FOLLOWING MUST GO ONE CHUNK AT A TIME #
########################## 
########################## 

library("filehash")
filehashOption("DB1")
dbCreate("t.ass")
db <- dbInit("t.ass", type="DB1")

# ok I need to go through this step-by-step where x is a manageable size still.
x=twit[[1]][1:500]
# So, if i go 500 at a time. well hell just make 500 chunsize then.
# so that will give me 4720 chunks, and take 16520 HOURS to process lol. so 3 years. Yeah that's not real tenable.

assocsToDB<-function(x){
tdm <- makeTDM(x)
# Create a matrix of associations.
ass<-lapply(dimnames(tdm)$Terms,FUN=function(x){findAssocs(tdm,x,0)})
# OK this above step is taking quite a while on only 1764 rows, 500 docs, so scalability may be nil. Thus the problem is find assoc? Perhaps I need to do findAssoc only after I have trimmed words found less often. Move on to the trigram database.
# so that took maybe 3 minutes. 
#clean out nulls. NOTE - why do i have stuff separated by periods though??
ass[which(lapply(1:length(ass),FUN=function(x){is.null(dimnames(ass[[x]]))==1})==TRUE)]=NULL
# that took nothing

#########################################
# Create filehash database
# 2 is the key word, 1 are the values.
# so names will be dimnames(t.ass[[x]][1])
# values will be: t.ass[[1]][1:length(t.ass[[1]])]

# Start the clock!
ptm <- proc.time()
lapply(1:length(ass),FUN=function(x){
key=dimnames(ass[[x]])[[2]]
new=ass[[x]][1:length(ass[[x]])]
names(new)=dimnames(ass[[x]])[[1]]

if(dbExists(db,key)==TRUE)
{
existing=db[[key]]
db[[key]]=c(new,existing)
}
else
{
db[[key]]=new
}
}) 
# 37 secs
# 1MB database.
# Stop the clock
proc.time() - ptm
}

# OK so.. 

# We should test this first on 1, 20000 lines might already be too much?
assocsToDB(twit[[1]][1:100])
# ok this worked, but below does not, so what is the diff?
assocsToDB(twit[[1]])
assocsToDB(twit[[1]][1:length(twit[[1]])])




assocsToDB(unlist(twit[[1]]))
# OK it starts with a TDM...

## OK this could be working, i mean it will take a while to make the TDM so let's give it a chance.
# BUT this may be flawed, documents should be sentences, not lines. i neef to unlist again? changr munge end to double unlist? before null tests?
# in fact all is built for single chunk so all needs an extra unlist!! lines are meaningless (?) perhaps not though

#ALSO: remove this? "athletes/celebr" 

# wow this is taking FOREVER. I'll give it until... 4 pm maybe (1 hour total?) and then I may need to end it. I should have tested it first on a smaller set... The chunk thing that is.
# I don't really understand either why it's not expanding in size in the t.ass file...

#lapply(twit,assocsToDB)


