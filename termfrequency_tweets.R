library(tidyverse)
library(readxl)
library(dplyr)
library(tm)
library(qdap)

#upload & combine csv files
setwd("~/GitHub/twint/outputs")
cocacola = read_csv("cocacola.csv")

#cocacola
#shell
#vw
#unilever
#starbucks
#nestle
#mcdonalds
#hm
#exxonmobil

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
  #corpus = tm_map(corpus, stemDocument, language = "english")   ##Not relevant for Scraping
  corpus = tm_map(corpus, removeWords,c(stopwords("en"),"greenwashing","cocacola","nestlé"))
  return(corpus)
}
cocacola_clean_corpus = clean_corpus(cocacola_tweets_corpus)

#Word Frequency
cocacola_tdm = TermDocumentMatrix(cocacola_clean_corpus)
cocacola_m = as.matrix(cocacola_tdm)
cocacola_term_frequency = rowSums(cocacola_m)
cocacola_term_frequency = sort(cocacola_term_frequency, decreasing = TRUE)
top30_cocacola = data.frame(cocacola_term_frequency[1:30])
#barplot(cocacola_term_frequency[1:30], col = "tan", las = 2)

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
