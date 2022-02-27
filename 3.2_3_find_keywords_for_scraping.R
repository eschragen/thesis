library(tidyverse)
library(readxl)
library(dplyr)
library(tm)
library(qdap)

##run for every company seperately & identify environment-related keywords/hashtags
##results in "3.2_1_tweet_scraping"
#cocacola
#shell
#vw
#unilever
#starbucks
#nestle
#mcdonalds
#hm
#exxonmobil

#upload & combine csv files
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs")
cocacola = read_csv("cocacola_greenwashing.csv")

#extract unique tweets in English
cocacola_tweets = cocacola %>%
  filter(language == "en") %>%
  select(tweet) %>%
  distinct()

#Preprocessing
cocacola_tweets_vector = VectorSource(cocacola_tweets)
cocacola_tweets_corpus = VCorpus(cocacola_tweets_vector)
clean_corpus = function(corpus){
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords,c(stopwords("en")))
  return(corpus)
}
cocacola_clean_corpus = clean_corpus(cocacola_tweets_corpus)

#Word Frequency
cocacola_tdm = TermDocumentMatrix(cocacola_clean_corpus)
cocacola_m = as.matrix(cocacola_tdm)
cocacola_term_frequency = rowSums(cocacola_m)
cocacola_term_frequency = sort(cocacola_term_frequency, decreasing = TRUE)
top30_cocacola = data.frame(cocacola_term_frequency[1:30])

#Hashtags
cocacola_hashtags = cocacola %>%
  filter(language == "en") %>%
  select(hashtags) %>%
  distinct()
cocacola_hashtags_clean = data.frame(strip(cocacola_hashtags))

cocacola_hashtags_vector = VectorSource(cocacola_hashtags_clean)
cocacola_hashtags_corpus = VCorpus(cocacola_hashtags_vector)
cocacola_clean_corpus_hashtags = clean_corpus(cocacola_hashtags_corpus)

cocacola_hashtags_tdm = TermDocumentMatrix(cocacola_clean_corpus_hashtags)
cocacola_m_hash = as.matrix(cocacola_hashtags_tdm)
cocacola_term_frequency_hash = rowSums(cocacola_m_hash)
cocacola_term_frequency_hash = sort(cocacola_term_frequency_hash, decreasing = TRUE)
top30_cocacola_hash = data.frame(cocacola_term_frequency_hash[1:30])
