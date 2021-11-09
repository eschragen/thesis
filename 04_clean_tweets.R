#libraries
library(tm)
library(tidyverse)
library(textclean)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(textstem)
options(scipen=999) #avoid scientific notations (e.g. e+18)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df")

#create subset of 1,000 tweets
df_subset = df %>% sample_n(1000)
tweets_subset = df_subset %>% select(id, tweet) 

#Cleaning
tweets_subset$tweet = iconv(tweets_subset$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
tweets_subset$tweet = gsub("<[^>]+>", "", tweets_subset$tweet) #Remove emojis
tweets_subset$tweet = gsub("http.+ |http.+$", "", tweets_subset$tweet) #Remove URLs
tweets_subset$tweet = gsub("#", "", tweets_subset$tweet) #Remove hashtags
tweets_subset$tweet = gsub("@\\S+", "", tweets_subset$tweet) #Remove Mentions
tweets_subset$tweet = replace_internet_slang(tweets_subset$tweet) #Replace Slang Words
tweets_subset$tweet = gsub("[ |\t]{2,}", "", tweets_subset$tweet) #Remove tabs
tweets_subset$tweet = str_trim(tweets_subset$tweet, side = "both") #Remove unnecessary whitespace
tweets_subset$tweet = replace_word_elongation(tweets_subset$tweet) #Replace word elongation
tweets_subset$tweet = replace_contraction(tweets_subset$tweet) #Replace contraction

#remove rows with empty tweets
tweets_subset[tweets_subset==""] = NA
tweets_subset = tweets_subset[complete.cases(tweets_subset),]

#remove duplicates
tweets_subset = tweets_subset %>% mutate(duplicate = duplicated(tweet)) %>% filter(duplicate == FALSE) %>% select(-duplicate)

#create corpus
tweets_corpus = VCorpus(VectorSource(tweets_subset$tweet))

clean_corpus = function(corpus){
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower), lazy=TRUE)
  return(corpus)
}

tweets_corpus_clean = clean_corpus(tweets_corpus)

tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, c(stopwords("en"), "vw", "volkswagen", "shell", "starbucks", "nestle", "mcdonalds","mcdonald", "mc donald's", "ikea", "hm", "exxonmobil", "exxon mobil", "coca cola", "cocacola", "unilever", "amp"))
tweets_corpus_clean = tm_map(tweets_corpus_clean , stripWhitespace)

#get content of corpus
content = as.data.frame(sapply(tweets_corpus_clean, function(x){x$content}))     #transponse (t) when you apply lemmatization
content = cbind(tweets_subset[,1], content[,1])
colnames(content) = c("id", "tweet")


## create wordcloud
# set.seed(1234)
# palet  = brewer.pal(8, 'Dark2')
# wordcloud(tweets_corpus_clean, min.freq = 10, scale = c(4, 0.2) , random.order = TRUE, col = palet)


