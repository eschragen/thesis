#libraries
library(tm)
library(tidyverse)
library(dplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)

source("~/GitHub/thesis/04_clean_tweets.R")

#prepare data for topic modeling: document-term matrix
dtm = DocumentTermMatrix(tweets_corpus_clean)

#Each row of the input matrix needs to contain at least one non-zero entry
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]

####DATA VISUALIZATION####
##show most frequent words
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
#### ####

#LDA topic modeling
#try with k = 5
lda = LDA(dtm, k = 5, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88), 
                         best = TRUE, thin = 500, burnin = 4000, 
                         iter = 2000))

#Top 10 terms or words under each topic
top10terms = as.matrix(terms(lda,10))
top10terms

#Topics found out by the model
lda.topics = as.matrix(topics(lda))
summary(as.factor(lda.topics))

#Get probabilities for each topic
topicprob = as.matrix(lda@gamma)

#Combine content of tweet with Topic Assignment
tweets_topics = cbind(content, lda.topics)
tweets_topics = tweets_topics %>% select(V1, lda.topics) 
colnames(tweets_topics) = c("tweet", "topic")
rownames(tweets_topics) = c(1:nrow(tweets_topics))

ggplot(tweets_topics, aes(x = lda.topics)) + geom_bar()
