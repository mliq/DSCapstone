# Merge trigram sets to 1.R

setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(data.table)

b=readRDS("b.Tfreq4.RDS")
n=readRDS("n.Tfreq4.RDS")
t=readRDS("t.Tfreq6.RDS")

# Join two tables and match the keys, summing the values:
# DTx=DT

DT=merge(n,DTx,all=TRUE)
DT[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = grams]
DT[,c("counts.x","counts.y") := NULL]

saveRDS(DT,"b4.t6.n4.Tfreq.RDS")
#help(":=")
