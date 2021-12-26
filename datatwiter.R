library(twitteR)
library(ROAuth)
library(tm)
library(rtweet)
library(wordcloud2)
library(wordcloud)
library(openssl)
library(httpuv)
library(base64enc)
library(httr)

CUSTOMER_KEY <- "njqvO4xMoofljmDQBqyPfqjXo"
CUSTOMER_SECRET <- "a8Yf3ipR7xrIPphZ45l3LQuyi9K6f1jAzP4eIkg4X9z0OrujTF"
ACCESS_TOKEN <- "2164542606-znR5kpn4nTuzWT6Vk3QINbaq4UTrJAc4J3fM4AG"
ACCESS_SECRET <- "HwJGqyQouJ11nbWKXBySj5kE3HGcIQL3BeLuIC2qzyqs5"

#Connects you to Twitter
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_SECRET)

#ambil data dari twitter
tweetVaksin<-searchTwitter("vaksin sinovac", n=1500, retryOnRateLimit = 10e3)

#save data mentah
saveRDS(tweetVaksin, file = 'data_mentah.rds')

#load data set
tweetVaksin<-readRDS('data_mentah.rds')
tweetMentah=twListToDF(tweetVaksin)
View(tweetMentah)

##visualisasi time series 
ts_plot(tweetMentah, "3 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frekuensi tweet tentang vaksin sinovac selama 9 hari kebelakang",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#lihat data mentah
View(tweetMentah)

#cleaning data
#hanya ambil tweet saja
komen<-tweetMentah$text
komenc<-Corpus(VectorSource(komen))

#hapus tanda baca, link url, huruf aneh, dan emoji
removeURL <-function(x) gsub("http[^[:space:]]*", "", x)
twitclean <-tm_map(komenc,removeURL)

removeRT<-function(y) gsub("RT", "", y)
twitclean<-tm_map(twitclean,removeRT)

removeUN<-function(z) gsub("@\\w+", "", z)
twitclean<-tm_map(twitclean,removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean<-tm_map(twitclean, removePunctuation)
twitclean<-tm_map(twitclean, tolower)

#membuat nilai untuk masing-masing kata
{
  dtm<-TermDocumentMatrix(twitclean)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  tweetMentah<-data.frame(word=names(v),freq=v)
}
head(tweetMentah,n=10)

wordcloud2(tweetMentah,shape = "cloud",
           backgroundColor = "black",
           color = 'random-light',
           size = 0.5)

## save data
dataframe<-data.frame(text=unlist(sapply(twitclean,'[')),stringsAsFactors = F)
View(dataframe)

write.csv(dataframe , "data_bersih.csv")

