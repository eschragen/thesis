#libraries
library(tm)
library(tidyverse)
library(dplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(textmineR)
library(ldatuning)

##Run Topic Modeling without POS cleaned data
# source("~/GitHub/thesis/04_clean_tweets.R")

#Run Topic Modeling with POS cleaned data
source("~/GitHub/thesis/05_1_POS_Tagging.R")

#Create Document Term Matrix
dtm = DocumentTermMatrix(tweets_corpus_stemmed)

##Each row of the input matrix needs to contain at least one non-zero entry
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]

####EXPLORE MOST FREQUENT WORDS####
#show most frequent words
# freq = colSums(as.matrix(dtm))
# ord = order(freq, decreasing = TRUE)
# freq[head(ord, n = 20)]
# 
# plot = data.frame(words = names(freq), count = freq)
# plot = subset(plot, plot$count > 50) #creating a subset of words having more than 100 frequency
# ggplot(data = plot, aes(reorder(words,count), count)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 50 times') +
#   coord_flip() + labs(x = "words")

##find associations mor most frequent words (e.g. "plastic", "water")
# findAssocs(dtm, "plastic", 0.2)
# findAssocs(dtm, "water", 0.2)


####MODEL TUNING ####
##determine number of topics (k)

result = FindTopicsNumber(
   dtm = dtm,
   topics = seq(from = 2, to = 10, by = 1),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control = list(seed = 12345),
   mc.cores = 2L,
   verbose = TRUE
 )

 FindTopicsNumber_plot(result)
# #CaoJuan yields best result at k = 5 (other methods don't yield any peak)

# #coherence score: choose k peak before major drop

# #perplexity score: find elbow
mod_log_lik = numeric(10)
mod_perplexity = numeric(10)
for (i in 2:10) {
  mod = LDA(dtm, k = i, method = "Gibbs",
            control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
  mod_log_lik[i] = logLik(mod)
  mod_perplexity[i] = perplexity(mod, dtm)
}

plot(mod_perplexity)


####Run LDA####
lda = LDA(dtm, k = 5, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88), 
                         best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# "burnin": number of omitted Gibbs iterations at beginning: they most likely do not correctly reflect the properties of distribution
# "thin": number of omitted in-between Gibbs iterations:  prevent correlations between samples during the iteration

#TO DO: Try other methods


#Top 10 terms or words under each topic
top20terms = as.matrix(terms(lda,20))
top20terms

#Topics found out by the model
lda.topics = as.data.frame(topics(lda))
table(lda.topics)

#Get probabilities for each topic / word
# topicprob = as.matrix(lda@gamma)
# word_topicprob = tidy(lda, matrix = "beta")

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
content = cbind(content, as.data.frame(doc.length))
content = content %>% filter(doc.length != 0) %>% select(-doc.length)
#Combine
tweets_topics = cbind(content, lda.topics)
colnames(tweets_topics) = c("tweet_cleaned", "id", "topic")

#convert character of id to numeric
tweets_topics$id = as.numeric(tweets_topics$id)

#Add Topic Assignment to original df
df_subset = df_subset %>% left_join(tweets_topics, by = "id") %>% filter(!is.na(topic))

#Show Topics of Companies
company_topic = df_subset %>% group_by(company) %>% count(topic)
ggplot(company_topic , aes(fill=company, y=n, x=topic)) + 
  geom_bar(position="dodge", stat="identity")
