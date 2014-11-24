## function to combine data tables ##

library(data.table)

#### TRIGRAMS ####
## Twitter ## 
Tfreq.t=readRDS("t.Tfreq4.RDS")
## Blogs ## 
Tfreq.b=readRDS("b.Tfreq4.RDS")
## News ## 
Tfreq.n=readRDS("n.Tfreq4.RDS")

#### MERGE BLOG AND TWITTER PREDICTION TABLES ####
Tpred.bt=merge(Tfreq.t,Tfreq.b,all=TRUE,suffixes=c(".t",".b"))
Tpred.bt[,counts:=(sum(counts.t,counts.b,na.rm=TRUE)), by = grams]
Tpred.bt[,c("counts.t","counts.b") := NULL]
Tpred.bt=Tpred.bt[order(-counts)]

#### MERGE NEWS TO BLOG/TWIT PREDICTION TABLE ####
Tpred.nbt=merge(Tfreq.n,Tpred.bt,all=TRUE,suffixes=c(".n",".bt"))
Tpred.nbt[,counts:=(sum(counts.n,counts.bt,na.rm=TRUE)), by = grams]
Tpred.nbt[,c("counts.n","counts.bt") := NULL]
Tpred.nbt=Tpred.nbt[order(-counts)]

#### CALCULATE PROBABILITIES ####

# total count of trigrams
nbtTotal=Tpred.nbt[,sum(counts)]

# Calculate probabilities:
Tpred.nbt[,probability:=counts/nbtTotal]
# 406 MB Tpred.nbt
# saveRDS(Tpred.nbt, file="Tpred.nbt.RDS")

#### BIGRAMS ####
## Twitter ## 
Bfreq.t=readRDS("t.Bfreq4.RDS")
## Blogs ## 
Bfreq.b=readRDS("b.Bfreq4.RDS")
## News ## 
Bfreq.n=readRDS("n.Bfreq4.RDS")

#### MERGE BLOG AND TWITTER PREDICTION TABLES ####
Bpred.bt=merge(Bfreq.t,Bfreq.b,all=TRUE,suffixes=c(".t",".b"))
Bpred.bt[,counts:=(sum(counts.t,counts.b,na.rm=TRUE)), by = grams]
Bpred.bt[,c("counts.t","counts.b") := NULL]
Bpred.bt=Bpred.bt[order(-counts)]

#### MERGE NEWS TO BLOG/TWIT PREDICTION TABLE ####
Bpred.nbt=merge(Bfreq.n,Bpred.bt,all=TRUE,suffixes=c(".n",".bt"))
Bpred.nbt[,counts:=(sum(counts.n,counts.bt,na.rm=TRUE)), by = grams]
Bpred.nbt[,c("counts.n","counts.bt") := NULL]
Bpred.nbt=Bpred.nbt[order(-counts)]

#### CALCULATE PROBABILITIES ####

# total count of trigrams
BnbtTotal=Bpred.nbt[,sum(counts)]

# Calculate probabilities:
Bpred.nbt[,probability:=counts/BnbtTotal]

# saveRDS(Bpred.nbt, file="Bpred.nbt.RDS")

#### UNIGRAMS ####
## Twitter ## 
Ufreq.t=readRDS("t.Ufreq4.RDS")
## Blogs ## 
Ufreq.b=readRDS("b.Ufreq4.RDS")
## News ## 
Ufreq.n=readRDS("n.Ufreq4.RDS")

#### MERGE BLOG AND TWITTER PREDICTION TABLES ####
Upred.bt=merge(Ufreq.t,Ufreq.b,all=TRUE,suffixes=c(".t",".b"))
Upred.bt[,counts:=(sum(counts.t,counts.b,na.rm=TRUE)), by = grams]
Upred.bt[,c("counts.t","counts.b") := NULL]
Upred.bt=Upred.bt[order(-counts)]

#### MERGE NEWS TO BLOG/TWIT PREDICTION TABLE ####
Upred.nbt=merge(Ufreq.n,Upred.bt,all=TRUE,suffixes=c(".n",".bt"))
Upred.nbt[,counts:=(sum(counts.n,counts.bt,na.rm=TRUE)), by = grams]
Upred.nbt[,c("counts.n","counts.bt") := NULL]
Upred.nbt=Upred.nbt[order(-counts)]

#### CALCULATE PROBABILITIES ####

# total count of trigrams
UnbtTotal=Upred.nbt[,sum(counts)]

# Calculate probabilities:
Upred.nbt[,probability:=counts/UnbtTotal]

# saveRDS(Upred.nbt, file="Upred.nbt.RDS")

## MEMSIZE: nearly 1 GB with no cleanup. (so could easily go 4x bigger...) OH but this is trigrams only!
# print(object.size(x=lapply(ls(), get)), units="Mb")
######################
#ISSUES I NOTICE WITH INITIAL TRANSFORMATIONS:
## Remove | } { 
## apostrophe: 'i don t' is top trigram! 'i can t' also 'i m not'

# crap i don t is still there... why.
# i m not also.
# hm why not just leave apostrophe's IN then??
# maybe stemming screws it up?
# i don't know but maybe I should use the remove punctuation function? 
# also didn t
# now 'i don t' is there, 
# 'it s a'
# 'i m not'
# 'i didn t'
# but 'i dont know' worked.
######################
