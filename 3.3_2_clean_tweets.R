#libraries
library(tm)
library(tidyverse)
library(textclean)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(textstem)
options(scipen=999) 
library(qdapRegex)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
df = read_csv("df_nonequal_size_subsetVW.csv")

tweets_subset = df %>% select(id, tweet) 

#remove duplicates
tweets_subset = tweets_subset %>% mutate(duplicate = duplicated(tweet)) %>% filter(duplicate == FALSE) %>% select(-duplicate)

#Cleaning
tweets_subset$tweet = iconv(tweets_subset$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
tweets_subset$tweet = gsub("(http[^ ]*)|(www.[^ ]*)", "", tweets_subset$tweet) #Remove URLs
tweets_subset$tweet = gsub("#\\S+", "", tweets_subset$tweet) #Remove hashtags
tweets_subset$tweet = gsub("@\\S+", "", tweets_subset$tweet) #Remove Mentions
tweets_subset$tweet = replace_internet_slang(tweets_subset$tweet) #Replace Slang Words
tweets_subset$tweet = gsub("[ |\t]{2,}", "", tweets_subset$tweet) #Remove tabs
tweets_subset$tweet = str_trim(tweets_subset$tweet, side = "both") #Remove unnecessary whitespace
tweets_subset$tweet = replace_word_elongation(tweets_subset$tweet) #Replace word elongation
tweets_subset$tweet = replace_contraction(tweets_subset$tweet) #Replace contraction

#remove rows with empty tweets
tweets_subset[tweets_subset==""] = NA
tweets_subset = tweets_subset[complete.cases(tweets_subset),]

#create corpus
tweets_corpus = VCorpus(VectorSource(tweets_subset$tweet))

clean_corpus = function(corpus){
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower), lazy=TRUE)
  return(corpus)
}

tweets_corpus_clean = clean_corpus(tweets_corpus)

##Remove Words (Stopwords, Custom stopwords)
tweets_corpus_stemmed = tm_map(tweets_corpus_clean, removeWords, c("vw", "volkswagen", "shell", "starbucks", "nestle", "mcdonalds","mcdonald", "mc donald's", "ikea", "hm", "exxonmobil", "exxon mobil", "coca cola", "cocacola", "unilever", "amp", "aamp", "hampm", "actually"))
tweets_corpus_stemmed = tm_map(tweets_corpus_stemmed, removeWords, stopwords("en")) 
##Apply Word Stemming / Lemmatization
tweets_corpus_stemmed = tm_map(tweets_corpus_stemmed, stemDocument)
tweets_corpus_stemmed = tm_map(tweets_corpus_stemmed , stripWhitespace)

save.image("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints/tweets_decreasedVW_stemmed.RData")


