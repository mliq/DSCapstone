----
Accuracy Measures
# 0.1593463 for t.Tfreq6.shrink.RDS : ok no drop that's cool...
# 0.1545497 for b.Tfreq4.shrink.RDS : some drop. darn.

# 0.063259 for t.Tfreq4.RDS
# 0.1593463 for t.Tfreq6.RDS

# 0.1361028 for n.Tfreq4.RDS
# 0.1737417 for b.Tfreq4.RDS

# history=words[(length(words)-1):length(words)]
# nMin1=words[length(words)]
# history=paste(as.character(history),collapse=' ')


# # Make prediction list of matches:
# Tpred=data.table(Tfreq[grep(paste0("^",history," "),Tfreq$grams),][order(-counts)])

# # Print out top 5 possibilities:
# print(Tpred[1:25])
# Tpred<<-Tpred
# }

# # Find function
# find=function(x){
# Tpred[grep(x,Tpred$grams),]
# }

# Calculate Probabilities?
# Edit trigrams to just the prediction and the count and the probabilities
# Tpred=Tpred[,{s=strsplit(grams," ");list(prediction=unlist(s)[c(FALSE,FALSE,TRUE)],counts=counts,probability=probability)}]

# Stop the clock
proc.time() - ptm # 1e6 twit: 35.15 secs
