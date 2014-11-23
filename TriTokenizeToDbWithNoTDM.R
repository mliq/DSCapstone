# database creation (CHANGE FILENAME!)
library("filehash")
filehashOption("DB1")
dbCreate("t.tri")
db <- dbInit("t.tri", type="DB1")

library(RWeka)
TgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

x=twit[[1]][1:500]
x=unlist(x)
# 914 sentences
test<-TgramTokenizer(x)
# 4460 trigrams.
# set as names all with 1 as the value.
# But what if they are more than one? That's where tdm comes in...
l=c(rep(1,length(test)))
names(l)=test
# ok...



trigramToDB<-function(x){
if(dbExists(db,x)==TRUE)
{db[[x]]=(db[[x]]+1)}
else
{db[[x]]=1}
}


# add up in database
lapply(tes,FUN=function(x){
key=names(counts[x])
c=counts[x]
if(dbExists(db,key)==TRUE)
{
existing=db[[key]]
db[[key]]=(existing+c)
}
else
{
db[[key]]=c
}
})
}

ptm <- proc.time()
counts=unlist(lapply(unlist(test),trigramCount))
proc.time() - ptm #270.25 

counts=list()
ptm <- proc.time()
counts=list(unlist(lapply(t,trigramCount)))
proc.time() - ptm #270.25 

counts=llply(test,trigramCount)

t<-list(test)
t<-unlist(t)

#counts=unlist(lapply(unlist(test),trigramCount))
#lapply(test,trigramToDB)
#trigramToDB(test)

# Make a list:
trigramCount<-function(x){
if(x %in% names(counts)){
counts[[x]]=counts[[x]]+1
}
else{
counts[[x]]=1
}
return(counts)
}

ptm <- proc.time()
lapply(1:length(test),trigramCount)
proc.time() - ptm #270.25 



counts=trigramCount("cats")
# ok finally POS works
counts=trigramCount("cats")
# I have no idea why above does not work, but it does not.
trigramCount<-function(x){
counts$x=1
}

###########################
# Try again using DumpList after all done in RAM
