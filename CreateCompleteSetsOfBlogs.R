### CREATE TRAINING SET ###
ptm <- proc.time()
# SETUP #
gc()
setwd("C:/Users/Michael/SkyDrive/Code/GitHub/DSCapstone/Coursera-SwiftKey/final/en_US")

# Read in
text<-readLines("en_US.blogs.txt")
#1010242
set1=text[0:336740]
set2=text[336741:673481]
set3=text[673482:1010242]

set1=iconv(set1, to='ASCII', sub=' ')
set2=iconv(set2, to='ASCII', sub=' ')
set3=iconv(set3, to='ASCII', sub=' ')

saveRDS(set1, file="b.1of3.RDS")
saveRDS(set2, file="b.2of3.RDS")
saveRDS(set3, file="b.3of3.RDS")