### CREATE TRAINING SET ###
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

# Read in
text<-readLines("en_US.twitter.txt")
#
set1=text[0:1000000]
set2=text[1000001:2000000]
set3=text[2000001:2360148]

set1=iconv(set1, to='ASCII', sub=' ')
set2=iconv(set2, to='ASCII', sub=' ')
set3=iconv(set3, to='ASCII', sub=' ')

saveRDS(set1, file="t.1of3.RDS")
saveRDS(set2, file="t.2of3.RDS")
saveRDS(set3, file="t.3of3.RDS")

