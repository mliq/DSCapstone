library(compiler)
library(data.table)

tfreq=readRDS("t.no4counts.RDS")

my.func <- function (dt) {
  dt.out <- dt[, lapply(.SD, sum), by = "a"]
  dt.out[, "count" := dt[, .N, by="a"]$N]
  dt.out
}


classify.<-cmpfun(classify)


	  total=length(z)-2
classify=function(y,z){
	  correct=0
	  lapply(1:total,FUN=function(x){
			# loop through sentence making bigram and answer, 
			bigram=paste(z[x], z[x+1])
			answer=paste(z[x+2])
			# then check answer against predicted answer.
			# Get answer
			Xpred=data.table(y[grep(paste0("^",bigram," "),y$grams),][order(-counts)])	
			# isolate the answer from prediction table.
			Xpred=unlist(strsplit(Xpred[1]$grams,"\\s+"))
			Xpred=Xpred[length(Xpred)]
			# Test equality of prediction to actual and counter for the accuracy measure
			if(!is.na(Xpred)){
			  if(Xpred==answer){correct=correct+1}	
			  correct<<-correct
			}
		})
		accuracy = correct/total
		return(accuracy)
	}