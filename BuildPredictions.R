####################################
#-BUILD PREDICTION LIST
####################################
# load already created database:
library(filehash)
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
db <- dbInit("t.ass", type="DB1")

# Get associations from a test set: #

# input test set
test<-fileMunge("test2.txt")
test.tdm<-makeTDM(test)

# Get list of sentence 1 terms:
test1<-dimnames(test.tdm[which(inspect(test.tdm[,1]!=0)),1])$Terms
# Fetch from DB frequent terms for first word of first sentence:
term=test1[2]

corrs=db[[term]]

# OK so corrs[[x]] gets just the number, 
# names(corrs)[x] gets the term.

####################################
####################################

inspect(test.tdm[,1])
which(inspect(test.tdm[,1]!=0))
inspect(test.tdm[which(inspect(test.tdm[,1]!=0)),1])
# OK that got me what I want now I want just the dimnames i guess.
length(dimnames(test.tdm[which(inspect(test.tdm[,1]!=0)),1])$Terms)
test1<-dimnames(test.tdm[which(inspect(test.tdm[,1]!=0)),1])$Terms
# What was the purpose of making TDM? Will I use frequency? Perhaps.
# So now i want to call up the file associations for each term, the more frequent may be given more weight later.
test1.db<-dbFetch(db,test1[2])
# 1. will need to deal with unfound terms like "ain't"
# 2. how do i get the correlation number? It does not appear to be here.

#########################################
#########################################
# Start the clock!
#ptm <- proc.time()

# Stop the clock
#proc.time() - ptm

#########################################
## Eliminate empties:
#dbReorganize(db)
#db <- dbInit("t.ass", type="DB1")
##
#########################################
