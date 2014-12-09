### CREATE TRAINING SET ###

# Read in
text<-readLines("en_US.blogs.txt")

# randomly choose rows and set to train
set.seed(42)
trainRows=sample(1:length(text),333333)
train=text[trainRows]
train=iconv(train, to='ASCII', sub=' ')

# Save to text file
sink('b.train4')
train
sink()

# Save to R object
saveRDS(train, file="b.train4.RDS")

#########
#TESTING#
#########
train[1]
text[trainRows[1]]
trainRows[1]

#####
#SAVE PREDICTIONS#
#####
# Save to text file
# Save to R object
# Blog time w/3.ProcessFix.R : 1938 secs
saveRDS(Bfreq, file="b.Bfreq4.RDS")
saveRDS(Tfreq, file="b.Tfreq4.RDS")
saveRDS(Ufreq, file="b.Ufreq4.RDS")
# News time w/3.ProcessFix.R : 1778 secs
saveRDS(Bfreq, file="n.Bfreq4.RDS")
saveRDS(Tfreq, file="n.Tfreq4.RDS")
saveRDS(Ufreq, file="n.Ufreq4.RDS")
# Twit time w/3.ProcessFix.R : 827 secs
saveRDS(Bfreq, file="t.Bfreq4.RDS")
saveRDS(Tfreq, file="t.Tfreq4.RDS")
saveRDS(Ufreq, file="t.Ufreq4.RDS")



# 1e6 twitter lines: 5095.00 85 min.
# object.size(Tfreq) 405358064 bytes
saveRDS(Tfreq, file="t.Tfreq6.RDS")
# Length:5266289
# Tfreq[counts>1] 791025

# Save to text file
options("max.print"=10000000)
sink('t.Tfreq6.txt')
Tfreq[order(-counts)]
sink()
