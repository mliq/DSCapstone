" ˆšBarbara Trelstad  "

comments:
# 2
I have been going nuts trying to fix strange characters in the text once it becomes a TDM. None of the methods here work completely for me.

For example, using the first 2000 lines of "en_US_news.txt", line 1854 is:
" ˆšBarbara Trelstad  "

If i try to use gsub to remove those characters it does not work.

What am I doing wrong?

# 1
I have been going nuts trying to fix strange characters in the text once it becomes a TDM. None of the methods here work. 

I can eliminate characters using gsub just fine from the text but the tm package functions seem to add new ones that cannot be removed, seemingly. 

But perhaps this is just an illusion of the way the inspect function outputs text on Windows? This thread indicates this: http://stackoverflow.com/questions/24920396/r-corpus-is-messing-up-my-utf-8-encoded-text.

For example, using the first 2000 lines of en_US_news.txt as a corpus and having cleaned it up, inspect prints out 
ˆšbarbara
as the first term. 

And there is apparently no way to remove those odd characters, since they are not present in the text or even the corpus from what I can see.

Finally, I think they are not really there, because when looking at the tdm or dtm in the environment window of RStudio it shows simply "barbara".

Any tips or clarity greatly appreciated!

## DISCARDED ##

```r
# Output to text for testing #
sink('analysis-output.txt')
cleanData
sink()
# END TESTER #

#
clean_data <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", lineNews)
clean_data <- gsub("'", "", clean_data)
clean_data <- gsub("™", " ", clean_data)
clean_data <- gsub("\"", " ", clean_data)
# good here but now numbers rule, also, hyphens
clean_data <- gsub("[0-9]", " ", clean_data)
clean_data <- gsub("$", " ", clean_data)
clean_data <- gsub("-", " ", clean_data)
# here we still have () $$$ % : . 
clean_data <- gsub("[()$%.*@^/šˆ`¨´?˜]", " ", clean_data)
clean_data <- gsub("š", " ", clean_data)
# note for '\' gsub in r you need to put "\\\\" http://r.789695.n4.nabble.com/gsub-with-unicode-and-escape-character-td3672737.html



#clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", lineNews)
# ok but not some others have appeared again... so somethinghere screws shit up.

# TESTER 2: http://stackoverflow.com/questions/24920396/r-corpus-is-messing-up-my-utf-8-encoded-text #
dtm <- DocumentTermMatrix(corpus)
Terms(dtm)

# TESTER #
clean_data

corpus <- Corpus(VectorSource(clean_data))
tdm<- TermDocumentMatrix(corpus)
inspect(tdm[1:20,]) #display top 20

# Output to text for testing #
sink('analysis-output.txt')
inspect(corpus)
sink()
# END TESTER #

#--GO STEP BY STEP ADDING BELOW -- #
clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", lineNews)
#clean_data <- gsub("[\002\020\023\177\003]", "", clean_data)
clean_data <- gsub("™", " ", clean_data)
clean_data <- gsub("˜", " ", clean_data)
clean_data <- gsub("“", " ", clean_data)
clean_data <- gsub("”", " ", clean_data)
#clean_data <- gsub("[:punct:]", "", clean_data)
# ELIMINATE DASHES / HYPHENS
clean_data <- gsub("-", " ", clean_data)
# error: clean_data <- gsub("\\", " ", clean_data)
## END REMOVAL OF STRANGE CHARACTERS##


```