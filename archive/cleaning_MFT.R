library(tidyverse)
df = data_eva %>% select(tweet_id, tweet, HarmVirtue, HarmVice, FairnessVirtue, FairnessVice, IngroupVirtue, IngroupVice, AuthorityVirtue, AuthorityVice, PurityVirtue, PurityVice, MoralityGeneral)

tweets_subset = df

#Cleaning
tweets_subset$tweet = iconv(tweets_subset$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
tweets_subset$tweet = gsub("http.+ |http.+$", "", tweets_subset$tweet) #Remove URLs
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

tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, c(stopwords("en"),"amp", "aamp",
                                                                 ' rt ', 'i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', "you're", "you've", "you'll",
                                                                 "you'd", 'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', "she's", 'her',
                                                                 'hers', 'herself', 'it', "it's", 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what',
                                                                 'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 'are', 'was', 'were',
                                                                 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the',
                                                                 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about',
                                                                 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from',
                                                                 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here',
                                                                 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other',
                                                                 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can',
                                                                 'will', 'just', 'don', "don't", 'should', "should've", 'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain',
                                                                 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 'hadn', "hadn't", 'hasn',
                                                                 "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', "mustn't", 'needn',
                                                                 "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren', "weren't", 'won', "won't",
                                                                 'wouldn', "wouldn't"))
tweets_corpus_clean = tm_map(tweets_corpus_clean , stripWhitespace)

#get content of corpus
content = as.data.frame(sapply(tweets_corpus_clean, function(x){x$content}))     #transponse (t) when you apply lemmatization
content = cbind(tweets_subset[,1], content[,1])
colnames(content) = c("id", "tweet")

##Apply Word Stemming / Lemmatization
tweets_corpus_stemmed = tm_map(tweets_corpus_clean, stemDocument)
##Or Lemmatization
# tweets_corpus_stemmed = tm_map(tweets_corpus_clean, lemmatize_strings)
# tweets_corpus_stemmed = tm_map(tweets_corpus_clean, PlainTextDocument)