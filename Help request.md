" ˆšBarbara Trelstad  "

comments:
I have been going nuts trying to fix strange characters in the text once it becomes a TDM. None of the methods here work. 

I can eliminate characters using gsub just fine from the text but the tm package functions seem to add new ones that cannot be removed, seemingly. 

But perhaps this is just an illusion of the way the inspect function outputs text on Windows? This thread indicates this: http://stackoverflow.com/questions/24920396/r-corpus-is-messing-up-my-utf-8-encoded-text.

For example, using the first 2000 lines of en_US_news.txt as a corpus and having cleaned it up, inspect prints out 
ˆšbarbara
as the first term. 

And there is apparently no way to remove those odd characters, since they are not present in the text or even the corpus from what I can see.

Finally, I think they are not really there, because when looking at the tdm or dtm in the environment window of RStudio it shows simply "barbara".

Any tips or clarity greatly appreciated!