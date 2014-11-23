### CREATE TRAINING SET ###

# Read in
text<-readLines("en_US.twitter.txt")

# randomly choose rows and set to train
set.seed(42)
trainRows=sample(1:length(text),100000)
train=text[t.train]
train=iconv(train, to='ASCII', sub=' ')

# Save to text file
sink('X.train')
train
sink()

# Save to R object
saveRDS(train, file="X.train.RDS")
