#libraries
library(tm)
library(tidyverse)
library(textclean)
library(wordcloud)
library(dplyr)
library(ggplot2)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df")
tweets = df %>% select(id, tweet) 

#create subset of 1,000 tweets
tweets_subset = tweets %>% sample_n(1000)

#extract emojis, url, hashtags, mentions
tweets_subset$tweet = gsub(">", "> ", tweets_subset$tweet)  # Add whitespace after every ">"
tweets_subset$tweet = gsub("<", " <", tweets_subset$tweet)  # Add whitespace before every "<"
tweets_subset$emojis = str_extract(tweets_subset$tweet, "<[^>]+>")
tweets_subset$url = str_extract(tweets_subset$tweet, "http.+")
tweets_subset$hashtag = str_extract(tweets_subset$tweet, "#\\S+")
tweets_subset$mention = str_extract(tweets_subset$tweet, "@\\S+")

#remove URL, mentions, emojis and hashtags
tweets_subset$tweet = iconv(tweets_subset$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
tweets_subset$tweet = gsub("<[^>]+>", "", tweets_subset$tweet) #Remove emojis
tweets_subset$tweet = gsub("http.+ |http.+$", "", tweets_subset$tweet) #Remove URLs
tweets_subset$tweet = gsub("#\\S+", "", tweets_subset$tweet) #Remove hashtags
tweets_subset$tweet = gsub("@\\S+", "", tweets_subset$tweet) #Remove Mentions
tweets_subset$tweet = gsub("[[:digit:]]+", "", tweets_subset$tweet) #Remove numbers
tweets_subset$tweet = gsub("[[:punct:]]+", "", tweets_subset$tweet) #Remove punctuation
tweets_subset$tweet = gsub("[ |\t]{2,}", "", tweets_subset$tweet) #Remove tabs
tweets_subset$tweet = str_trim(tweets_subset$tweet) #Remove unnecessary whitespace


tweets_subset$tweet = tolower(tweets_subset$tweet) #Replace to lower words
tweets_subset$tweet = replace_word_elongation(tweets_subset$tweet) #Replace word elongation
tweets_subset$tweet = replace_contraction(tweets_subset$tweet) #Replace contraction

#unique tweets
tweets_unique = unique(tweets_subset$tweet)

#create corpus
tweets_corpus = VCorpus(VectorSource(tweets_unique))

clean_corpus = function(corpus){
  # corpus = tm_map(corpus, removePunctuation)
  # corpus = tm_map(corpus, stripWhitespace)
  # corpus = tm_map(corpus, removeNumbers)
  # corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords,c(stopwords("en"),"vw", "volkswagen", "shell", "starbucks", 
                                       "nestle", "mcdonalds", "mc donald's", "ikea", "hm", "exxonmobil", 
                                       "coca cola", "unilever"))
  corpus = tm_map(corpus, stemDocument, language = "english") #tbd
  return(corpus)
}

tweets_corpus_clean = clean_corpus(tweets_corpus)

#get content of corpus
# out = as.data.frame(sapply(tweets_corpus_clean, function(x){x$content}))
# write.csv(out, file = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/corpus.csv")

#create wordcloud
# set.seed(1234)
# palet  = brewer.pal(8, 'Dark2')
# wordcloud(tweets_corpus_clean, min.freq = 20, scale = c(4, 0.2) , random.order = TRUE, col = palet)


