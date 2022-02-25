#libraries
library(tm)
library(tidyverse)
library(dplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(textmineR)
library(ldatuning)
library(hrbrthemes)
library(udpipe)

##Run Topic Modeling without POS cleaned data
# source("~/GitHub/thesis/04_clean_tweets.R")

# #Run Topic Modeling with POS cleaned data
# source("~/GitHub/thesis/04_1_POS_Tagging.R")

##Source really_stemmed

#get content of stemmed corpus

tweets_corpus_really_stemmed = tweets_corpus_stemmed



test = as.data.frame(c(1:length(tweets_corpus_stemmed$content)))
colnames(test) = "content"
for (i in 1:length(tweets_corpus_stemmed$content)) {
   test$content[i] = tweets_corpus_stemmed[["content"]][[i]][["content"]]
}


content_stemmed = as.data.frame(t(sapply(tweets_corpus_stemmed, function(x){x$content})))     #transponse (t) when you apply lemmatization
content_stemmed = cbind(tweets_subset[,c(1)], content_stemmed[,1])

content_stemmed = cbind(content[,1], test)
colnames(content_stemmed) = c("id","tweet_stemmed")



content_stemmed = content_stemmed %>% left_join(df, by = "id")
content_stemmed = content_stemmed[,c(1,2,22)]
colnames(content_stemmed) = c("id","tweet_stemmed", "company")


content_stemmed_subset = content_stemmed %>% group_by(company) %>% sample_n(100)
#write.csv(content_stemmed_subset, "content_stemmed_subset.csv")
subset_courpus = VCorpus(VectorSource(content_stemmed_subset$tweet_stemmed))

#Create Document Term Matrix
dtm = DocumentTermMatrix(subset_courpus)

#Each row of the input matrix needs to contain at least one non-zero entry
#memory.limit(9999999999)
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
# #Optional: Remove terms with low frequency
# dtm = dtm_remove_lowfreq(dtm, minfreq = 1)

####EXPLORE MOST FREQUENT WORDS####

## create wordcloud
library(wordcloud)
set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
wordcloud(tweets_corpus_stemmed, min.freq = 200, scale = c(4, 0.2) , random.order = TRUE, col = palet)

# # #show most frequent words
# freq = colSums(as.matrix(dtm))
# ord = order(freq, decreasing = TRUE)
# freq[head(ord, n = 20)]

 
# plot = data.frame(words = names(freq), count = freq)
# plot = subset(plot, plot$count > 50) #creating a subset of words having more than 100 frequency
# ggplot(data = plot, aes(reorder(words,count), count)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 50 times') +
#   coord_flip() + labs(x = "words")

##find associations of most frequent words (e.g. "plastic", "water")
# findAssocs(dtm, "greenwash", 0.2)
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
# #CaoJuan yields best result at k = 4 (other methods don't yield any peak)


# #coherence score: choose k peak before major drop


# #perplexity score: elbow at k = 5 
mod_log_lik = numeric(10)
mod_perplexity = numeric(10)
for (i in 2:10) {
  mod = LDA(dtm, k = i, method = "Gibbs",
            control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
  mod_log_lik[i] = logLik(mod)
  mod_perplexity[i] = perplexity(mod, dtm)
}

plot(mod_perplexity)
perplexity = as.data.frame(mod_perplexity)
perplexity$topics = as.numeric(rownames(perplexity))

ggplot(perplexity, aes(x = topics, y = mod_perplexity)) +
   geom_line() + geom_point() + ggtitle("Model Perplexity") + theme_ipsum() +
   scale_x_continuous(name="# Topics", limits=c(2, 10)) +
   scale_y_continuous(name="Perplexity")


####Run LDA####
lda = LDA(dtm, k = 5, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88), 
                         best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# "burnin": number of omitted Gibbs iterations at beginning: they most likely do not correctly reflect the properties of distribution
# "thin": number of omitted in-between Gibbs iterations:  prevent correlations between samples during the iteration

#Topics found out by the model
lda.topics = as.data.frame(topics(lda))
#table(lda.topics)

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




####VISUALIZATIONS/DESCRIBE TOPIC ASSIGNMENTS####
#Top 10 terms or words under each topic
# top20terms = as.matrix(terms(lda,20))
# top20terms
# 
# #Beta probabilities for each word
# topicprob = as.matrix(lda@gamma)
# word_topicprob = tidy(lda, matrix = "beta")
# 
# top_terms_per_topic = word_topicprob %>%
#    group_by(topic) %>%
#    slice_max(beta, n = 10) %>% 
#    ungroup() %>%
#    arrange(topic, -beta)
# 
# top_terms_per_topic %>%
#    mutate(term = reorder_within(term, beta, topic)) %>%
#    ggplot(aes(beta, term, fill = factor(topic))) +
#    geom_col(show.legend = FALSE) +
#    facet_wrap(~ topic, scales = "free") +
#    scale_y_reordered()
# 
# #Gamma probabilities for each topic of each company
# gamma = cbind(content, lda.topics, topicprob)
# gamma$id = as.numeric(gamma$id)
# gamma = df_subset %>% left_join(gamma, by = "id") 
# gamma = as.data.frame(cbind(gamma$company, gamma$`1`, gamma$`2`,gamma$`3`,gamma$`4`,gamma$`5`))
# colnames(gamma) = c("company", c(1:5))
# gamma = gamma %>% gather("topic", "gamma", 2:6)
# gamma$topic = as.numeric(gamma$topic)
# gamma$gamma = as.numeric(gamma$gamma)
# gamma %>%
#    mutate(company = reorder(company, gamma * topic)) %>%
#    ggplot(aes(factor(topic), gamma)) +
#    geom_boxplot() +
#    facet_wrap(~ company) +
#    labs(x = "topic", y = expression(gamma))
# 
# #Find topic that was most associated with company
# company_classifications = gamma %>%
#    group_by(company) %>%
#    slice_max(gamma) %>%
#    ungroup()
# company_classifications
# #Show topic assignments for each company
# company_topic = df_subset %>% group_by(company) %>% count(topic)
# #per topic
# ggplot(company_topic , aes(fill=company, y=n, x=topic)) + 
#    geom_bar(position="dodge", stat="identity")
# #per company
# ggplot(company_topic , aes(fill=topic, y=n, x=company)) + 
#    geom_bar(position="fill", stat="identity")

# #Optional: Replace Topic Numbers with Top 5 Terms
# top5termsPerTopic = terms(lda, 5)
# top5termsPerTopic = apply(top5termsPerTopic,2,paste, collapse = "|")
# top5termsPerTopic

