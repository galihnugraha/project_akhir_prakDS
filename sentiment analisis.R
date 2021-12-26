#sentiment anlysis
library(syuzhet)

#read file
text_df<-read.csv("data_bersih.csv", stringsAsFactors = FALSE)

#convert ke chracter vector
review <-as.character(text_df$text)

#sentiment scores
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')
s<-get_nrc_sentiment(review)

#combine text and sentiment columns
review_sentiment<-cbind(text_df$text,s)
View(review_sentiment)

#bar plot for sentiment
barplot(colSums(s),col = rainbow(10), ylab = 'Count', main = 'Sentiment Score For vaksin sinovac')

#hasil 
hasil=score.sentiment
