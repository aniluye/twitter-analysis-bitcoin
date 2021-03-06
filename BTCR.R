install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("RCurl")

library(twitteR)
library(ROAuth)
library(tm)
library(RCurl)

options(RCurlOptions = list(cainfo = system.file("CurlSSL","cacert.pem",package = "RCull")))

#API girisi

api_key <- "pO3*****yGScTC*****GhR"
api_secret <-"QBB******uCTQrFX2C*******syaC8wxA********Smv"
access_token <-"10***********w8Zx*********tDMYebK*****3S"
access_token_secret <-"CdD******CPXzH5Keztrt********Uc2epAht5"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Twitlerin indirilmesi 
btc <- searchTwitter("#Bitcoin",n=5000,lang = "en")

btcdf <- twListToDF(btc)

View(btcdf)

str(btcdf)

# CLEANING TWEETS

btcdf$text=gsub("&amp", "", btcdf$text)
btcdf$text = gsub("&amp", "", btcdf$text)
btcdf$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", btcdf$text)
btcdf$text = gsub("@\\w+", "", btcdf$text)
btcdf$text = gsub("[[:punct:]]", "", btcdf$text)
btcdf$text = gsub("[[:digit:]]", "", btcdf$text)
btcdf$text = gsub("http\\w+", "", btcdf$text)
btcdf$text = gsub("[ \t]{2,}", "", btcdf$text)
btcdf$text = gsub("^\\s+|\\s+$", "", btcdf$text)

btcdf$text <- iconv(btcdf$text, "UTF-8", "ASCII", sub="")

##MOST FREQUENT WORDS

library(tidyverse)
library(syuzhet)
library(tidytext)

btcdf2 <- btcdf %>%
  select(text) %>%
  unnest_tokens(word, text)
btcdf2 <- btcdf2 %>%
  anti_join(stop_words)


btcdfTn <- btcdf2 %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Bitcoin",
       subtitle = "Stop words removed from the list")





## SENTIMENT ANALYSIS 

btcdf3 <- btcdf

btcdf3 <- iconv(btcdf, from="UTF-8", to="ASCII", sub="")

btcdf3 <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",btcdf3)

btcdf3 <-gsub("@\\w+","",btcdf3)

library(syuzhet)
ew_sentiment<-get_nrc_sentiment((btcdf3))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
btcdfSn<- ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()





