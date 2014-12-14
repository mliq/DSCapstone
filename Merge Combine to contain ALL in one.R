# Merge trigram sets to 1.R

setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(data.table)

a=readRDS("b.all3.Tfreq.RDS")
b=readRDS("n.all3.Tfreq.RDS")
c=readRDS("t.all3.Tfreq.RDS")

DT=merge(a,b,all=TRUE)
DT[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = grams]
DT[,c("counts.x","counts.y") := NULL]

DTx=merge(DT,c,all=TRUE)
DTx[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = grams]
DTx[,c("counts.x","counts.y") := NULL]

saveRDS(DTx,"ALL.Tfreq.RDS")
