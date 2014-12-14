# Merge trigram sets to 1.R

setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")
library(data.table)

a=readRDS("t.TSet2.TFreq.RDS")
b=readRDS("t.Tfreq6.RDS")

DT=merge(a,b,all=TRUE)
DT[,counts:=sum(counts.x,counts.y,na.rm=TRUE), by = grams]
DT[,c("counts.x","counts.y") := NULL]

saveRDS(DT,"t1.t2.TFreq.RDS")
