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

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

####1. DETERMINE NUMBER OF TOPICS####
# ####unilever####
# content_stemmed_unilever = read.csv("content_stemmed_unilever.csv")
# #create corpus
# tweets_corpus_stemmed_unilever = VCorpus(VectorSource(content_stemmed_unilever$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_unilever)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_unilever = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_unilever = numeric(10)
# # mod_perplexity_unilever = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_unilever[i] = logLik(mod)
# #    mod_perplexity_unilever[i] = perplexity(mod, dtm)
# # }
# # perplexity_unilever = as.data.frame(mod_perplexity_unilever)
# # perplexity_unilever$topics = as.numeric(rownames(perplexity_unilever))
# 
# save.image(file = "unilever.RData")
# rm(list = ls())
# 
# 
# 
# 
# ####hm####
# content_stemmed_hm = read.csv("content_stemmed_hm.csv")
# #create corpus
# tweets_corpus_stemmed_hm = VCorpus(VectorSource(content_stemmed_hm$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_hm)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_hm = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_hm = numeric(10)
# # mod_perplexity_hm = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_hm[i] = logLik(mod)
# #    mod_perplexity_hm[i] = perplexity(mod, dtm)
# # }
# # perplexity_hm = as.data.frame(mod_perplexity_hm)
# # perplexity_hm$topics = as.numeric(rownames(perplexity_hm))
# 
# save.image(file = "hm.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####cocacola####
# content_stemmed_cocacola = read.csv("content_stemmed_cocacola.csv")
# #create corpus
# tweets_corpus_stemmed_cocacola = VCorpus(VectorSource(content_stemmed_cocacola$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_cocacola)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_cocacola = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
#  )
# # mod_log_lik_cocacola = numeric(10)
# # mod_perplexity_cocacola = numeric(10)
# # for (i in 2:10) {
# #   mod = LDA(dtm, k = i, method = "Gibbs",
# #             control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #   mod_log_lik_cocacola[i] = logLik(mod)
# #   mod_perplexity_cocacola[i] = perplexity(mod, dtm)
# # }
# # perplexity_cocacola = as.data.frame(mod_perplexity_cocacola)
# # perplexity_cocacola$topics = as.numeric(rownames(perplexity_cocacola))
# 
# save.image(file = "cocacola.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# ####exxonmobil####
# content_stemmed_exxonmobil = read.csv("content_stemmed_exxonmobil.csv")
# #create corpus
# tweets_corpus_stemmed_exxonmobil = VCorpus(VectorSource(content_stemmed_exxonmobil$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_exxonmobil)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_exxonmobil = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_exxonmobil = numeric(10)
# # mod_perplexity_exxonmobil = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_exxonmobil[i] = logLik(mod)
# #    mod_perplexity_exxonmobil[i] = perplexity(mod, dtm)
# # }
# # perplexity_exxonmobil = as.data.frame(mod_perplexity_exxonmobil)
# # perplexity_exxonmobil$topics = as.numeric(rownames(perplexity_exxonmobil))
# 
# save.image(file = "exxonmobil.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####ikea####
# content_stemmed_ikea = read.csv("content_stemmed_ikea.csv")
# content_stemmed_ikea = content_stemmed_ikea %>% filter(!grepl('actually',tweet_stemmed))
# 
# #create corpus
# tweets_corpus_stemmed_ikea = VCorpus(VectorSource(content_stemmed_ikea$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_ikea)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_ikea = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_ikea = numeric(10)
# # mod_perplexity_ikea = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_ikea[i] = logLik(mod)
# #    mod_perplexity_ikea[i] = perplexity(mod, dtm)
# # }
# # perplexity_ikea = as.data.frame(mod_perplexity_ikea)
# # perplexity_ikea$topics = as.numeric(rownames(perplexity_ikea))
# 
# save.image(file = "ikea.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####nestle####
# content_stemmed_nestle = read.csv("content_stemmed_nestle.csv")
# #create corpus
# tweets_corpus_stemmed_nestle = VCorpus(VectorSource(content_stemmed_nestle$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_nestle)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_nestle = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_nestle = numeric(10)
# # mod_perplexity_nestle = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_nestle[i] = logLik(mod)
# #    mod_perplexity_nestle[i] = perplexity(mod, dtm)
# # }
# # perplexity_nestle = as.data.frame(mod_perplexity_nestle)
# # perplexity_nestle$topics = as.numeric(rownames(perplexity_nestle))
# 
# save.image(file = "nestle.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####subset_small####
# content_stemmed_subset_small = read.csv("content_stemmed_subset_small.csv")
# #create corpus
# tweets_corpus_stemmed_subset_small = VCorpus(VectorSource(content_stemmed_subset_small$tweet_stemmed))
# #Create Document Term Matrix
# dtm = DocumentTermMatrix(tweets_corpus_stemmed_subset_small)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# doc.length = apply(dtm, 1, sum)
# dtm = dtm[doc.length > 0,]
# 
# #calculate scores for 2-10 topics
# result_subset_small = FindTopicsNumber(
#    dtm = dtm,
#    topics = seq(from = 2, to = 10, by = 1),
#    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#    method = "Gibbs",
#    control = list(seed = 12345),
#    mc.cores = 2L,
#    verbose = TRUE
# )
# # mod_log_lik_subset_small = numeric(10)
# # mod_perplexity_subset_small = numeric(10)
# # for (i in 2:10) {
# #    mod = LDA(dtm, k = i, method = "Gibbs",
# #              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
# #    mod_log_lik_subset_small[i] = logLik(mod)
# #    mod_perplexity_subset_small[i] = perplexity(mod, dtm)
# # }
# # perplexity_subset_small = as.data.frame(mod_perplexity_subset_small)
# # perplexity_subset_small$topics = as.numeric(rownames(perplexity_subset_small))
# 
# save.image(file = "subset_small.RData")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 


####subset####

load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/tweets_decreasedVW_stemmed.RData")
#Create Document Term Matrix
dtm = DocumentTermMatrix(tweets_corpus_stemmed)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)

ui = unique(dtm$i)
dtm.new = dtm[ui,]

#calculate scores for 2-10 topics
memory.limit(9999999999)
result_subset = FindTopicsNumber(
   dtm = dtm.new,
   topics = seq(from = 2, to = 100, by = 10),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control = list(seed = 12345),
   mc.cores = 2L,
   verbose = TRUE
)

beepr::beep(8)
# mod_log_lik_subset = numeric(10)
# mod_perplexity_subset = numeric(10)
# for (i in 2:10) {
#    mod = LDA(dtm, k = i, method = "Gibbs",
#              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
#    mod_log_lik_subset[i] = logLik(mod)
#    mod_perplexity_subset[i] = perplexity(mod, dtm)
# }
# perplexity_subset = as.data.frame(mod_perplexity_subset)
# perplexity_subset$topics = as.numeric(rownames(perplexity_subset))


save.image(file = "subset.RData")
rm(list = ls())


####shell####
content_stemmed_shell = read.csv("content_stemmed_shell.csv")
#create corpus
tweets_corpus_stemmed_shell = VCorpus(VectorSource(content_stemmed_shell$tweet_stemmed))
#Create Document Term Matrix
dtm = DocumentTermMatrix(tweets_corpus_stemmed_shell)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]

#calculate scores for 2-10 topics
result_shell = FindTopicsNumber(
   dtm = dtm,
   topics = seq(from = 2, to = 10, by = 1),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control = list(seed = 12345),
   mc.cores = 2L,
   verbose = TRUE
)
# mod_log_lik_shell = numeric(10)
# mod_perplexity_shell = numeric(10)
# for (i in 2:10) {
#    mod = LDA(dtm, k = i, method = "Gibbs",
#              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
#    mod_log_lik_shell[i] = logLik(mod)
#    mod_perplexity_shell[i] = perplexity(mod, dtm)
# }
# perplexity_shell = as.data.frame(mod_perplexity_shell)
# perplexity_shell$topics = as.numeric(rownames(perplexity_shell))

save.image(file = "shell.RData")
rm(list = ls())











# ####vw####
content_stemmed_vw = read.csv("content_stemmed_vw.csv")
#create corpus
tweets_corpus_stemmed_vw = VCorpus(VectorSource(content_stemmed_vw$tweet_stemmed))
#Create Document Term Matrix
dtm = DocumentTermMatrix(tweets_corpus_stemmed_vw)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
memory.limit(9999999999)
#calculate scores for 2-10 topics
result_vw = FindTopicsNumber(
   dtm = dtm,
   topics = seq(from = 2, to = 10, by = 1),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control = list(seed = 12345),
   mc.cores = 2L,
   verbose = TRUE
)
# memory.limit(9999999999)
# mod_log_lik_vw = numeric(10)
# mod_perplexity_vw = numeric(10)
# memory.limit(9999999999)
# for (i in 2:10) {
#    mod = LDA(dtm, k = i, method = "Gibbs",
#              control = list(alpha = 0.5, iter = 1000, seed=12345, thin =1))
#    mod_log_lik_vw[i] = logLik(mod)
#    mod_perplexity_vw[i] = perplexity(mod, dtm)
# }
# perplexity_vw = as.data.frame(mod_perplexity_vw)
# perplexity_vw$topics = as.numeric(rownames(perplexity_vw))

save.image(file = "vw.RData")
rm(list = ls())
























# ####2. DETERMINE NUMBER OF TOPICS MANUALLY####
# # ####unilever####
# load("unilever.RData")
# FindTopicsNumber_plot(result_unilever)
# # # ggplot(perplexity_unilever, aes(x = topics, y = mod_perplexity_unilever)) +
# # #     geom_line() + geom_point() + ggtitle("Model Perplexity unilever") + theme_ipsum() +
# # #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# # #     scale_y_continuous(name="Perplexity")
# # 
# # 
# # 
# # ####hm####
# load("hm.RData")
# FindTopicsNumber_plot(result_hm)
# # # ggplot(perplexity_hm, aes(x = topics, y = mod_perplexity_hm)) +
# # #     geom_line() + geom_point() + ggtitle("Model Perplexity hm") + theme_ipsum() +
# # #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# # #     scale_y_continuous(name="Perplexity")
# # 
# # 
# # 
# # 
# # ####cocacola####
# load("cocacola.RData")
# FindTopicsNumber_plot(result_cocacola)
# # # ggplot(perplexity_cocacola, aes(x = topics, y = mod_perplexity_cocacola)) +
# # #     geom_line() + geom_point() + ggtitle("Model Perplexity cocacola") + theme_ipsum() +
# # #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# # #     scale_y_continuous(name="Perplexity")
# # 
# # 
# # 
# # 
# # # ####exxonmobil####
# load("exxonmobil.RData")
# FindTopicsNumber_plot(result_exxonmobil)
# # ggplot(perplexity_exxonmobil, aes(x = topics, y = mod_perplexity_exxonmobil)) +
# #     geom_line() + geom_point() + ggtitle("Model Perplexity exxonmobil") + theme_ipsum() +
# #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# #     scale_y_continuous(name="Perplexity")
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # ####ikea####
# load("ikea.RData")
# FindTopicsNumber_plot(result_ikea)
# # ggplot(perplexity_ikea, aes(x = topics, y = mod_perplexity_ikea)) +
# #     geom_line() + geom_point() + ggtitle("Model Perplexity ikea") + theme_ipsum() +
# #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# #     scale_y_continuous(name="Perplexity")
# # 
# # # 
# # 
# # ####nestle####
# load("nestle.RData")
# FindTopicsNumber_plot(result_nestle)
# # ggplot(perplexity_nestle, aes(x = topics, y = mod_perplexity_nestle)) +
# #     geom_line() + geom_point() + ggtitle("Model Perplexity nestle") + theme_ipsum() +
# #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# #     scale_y_continuous(name="Perplexity")
# # # 
# # 
# # 
# 
# 
# 



####subset####
load("subset.RData")
FindTopicsNumber_plot(result_subset)
# ggplot(perplexity_subset, aes(x = topics, y = mod_perplexity_subset)) +
#     geom_line() + geom_point() + ggtitle("Model Perplexity subset") + theme_ipsum() +
#     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
#     scale_y_continuous(name="Perplexity")
# # 
# load("subset_small.RData")
# FindTopicsNumber_plot(result_subset_small)
# # ggplot(perplexity_subset, aes(x = topics, y = mod_perplexity_subset)) +
# #     geom_line() + geom_point() + ggtitle("Model Perplexity subset") + theme_ipsum() +
# #     scale_x_continuous(name="# Topics", limits=c(2, 10)) +
# #     scale_y_continuous(name="Perplexity")








####shell####
load("shell.RData")
FindTopicsNumber_plot(result_shell)
ggplot(perplexity_shell, aes(x = topics, y = mod_perplexity_shell)) +
    geom_line() + geom_point() + ggtitle("Model Perplexity shell") + theme_ipsum() +
    scale_x_continuous(name="# Topics", limits=c(2, 10)) +
    scale_y_continuous(name="Perplexity")




####vw####
load("vw.RData")
FindTopicsNumber_plot(result_vw)
ggplot(perplexity_vw, aes(x = topics, y = mod_perplexity_vw)) +
    geom_line() + geom_point() + ggtitle("Model Perplexity vw") + theme_ipsum() +
    scale_x_continuous(name="# Topics", limits=c(2, 10)) +
    scale_y_continuous(name="Perplexity")










# ####3. RUN TOPIC MODELING####
# # ####hm####
# 
# load("hm.RData")
# k_hm = 4
# 
# #Run LDA
# lda = LDA(dtm, k = k_hm, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_hm = cbind(content_stemmed_hm, as.data.frame(doc.length))
# content_stemmed_hm = content_stemmed_hm %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_hm = cbind(content_stemmed_hm, lda.topics)
# colnames(tweets_topics_hm) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_hm$id = as.numeric(tweets_topics_hm$id)
# write.csv(tweets_topics_hm, "topics_hm.csv")
# save.image(file = "hm_lda.RData")
# rm(list = ls())
# 
# 
# 
# # ####unilever####
# 
# load("unilever.RData")
# k_unilever = 4
# 
# #Run LDA
# lda = LDA(dtm, k = k_unilever, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_unilever = cbind(content_stemmed_unilever, as.data.frame(doc.length))
# content_stemmed_unilever = content_stemmed_unilever %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_unilever = cbind(content_stemmed_unilever, lda.topics)
# colnames(tweets_topics_unilever) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_unilever$id = as.numeric(tweets_topics_unilever$id)
# write.csv(tweets_topics_unilever, "topics_unilever.csv")
# save.image(file = "unilever_lda.RData")
# 
# rm(list = ls())
# 
# 
# 
# # ####cocacola####
# 
# load("cocacola.RData")
# k_cocacola = 5
# 
# #Run LDA
# lda = LDA(dtm, k = k_cocacola, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_cocacola = cbind(content_stemmed_cocacola, as.data.frame(doc.length))
# content_stemmed_cocacola = content_stemmed_cocacola %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_cocacola = cbind(content_stemmed_cocacola, lda.topics)
# colnames(tweets_topics_cocacola) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_cocacola$id = as.numeric(tweets_topics_cocacola$id)
# save.image(file = "cocacola_lda.RData")
# write.csv(tweets_topics_cocacola, "topics_cocacola.csv")
# 
# rm(list = ls())
# 
# beepr::beep(8)
# 
# # ####exxonmobil####
# 
# load("exxonmobil.RData")
# k_exxonmobil = 5
# 
# #Run LDA
# lda = LDA(dtm, k = k_exxonmobil, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_exxonmobil = cbind(content_stemmed_exxonmobil, as.data.frame(doc.length))
# content_stemmed_exxonmobil = content_stemmed_exxonmobil %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_exxonmobil = cbind(content_stemmed_exxonmobil, lda.topics)
# colnames(tweets_topics_exxonmobil) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_exxonmobil$id = as.numeric(tweets_topics_exxonmobil$id)
# save.image(file = "exxonmobil_lda.RData")
# 
# write.csv(tweets_topics_exxonmobil, "topics_exxonmobil.csv")
# rm(list = ls())
# 
# 
# 
# 
# 
# # ####ikea####
# 
# load("ikea.RData")
# k_ikea = 3
# 
# #Run LDA
# lda = LDA(dtm, k = k_ikea, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_ikea = cbind(content_stemmed_ikea, as.data.frame(doc.length))
# content_stemmed_ikea = content_stemmed_ikea %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_ikea = cbind(content_stemmed_ikea, lda.topics)
# colnames(tweets_topics_ikea) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_ikea$id = as.numeric(tweets_topics_ikea$id)
# save.image(file = "ikea_lda.RData")
# 
# write.csv(tweets_topics_ikea, "topics_ikea.csv")
# rm(list = ls())
# beepr::beep(8)
# 
# 
# 
# 
# # ####nestle####
# 
# load("nestle.RData")
# k_nestle = 4
# 
# #Run LDA
# lda = LDA(dtm, k = k_nestle, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_nestle = cbind(content_stemmed_nestle, as.data.frame(doc.length))
# content_stemmed_nestle = content_stemmed_nestle %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_nestle = cbind(content_stemmed_nestle, lda.topics)
# colnames(tweets_topics_nestle) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_nestle$id = as.numeric(tweets_topics_nestle$id)
# save.image(file = "nestle_lda.RData")
# 
# write.csv(tweets_topics_nestle, "topics_nestle.csv")
# rm(list = ls())
# 
# 
# 
# # ####subset_small####
# # 
# load("subset_small.RData")
# k_subset_small = 4
# 
# #Run LDA
# lda = LDA(dtm, k = k_subset_small, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_subset_small = cbind(content_stemmed_subset_small, as.data.frame(doc.length))
# content_stemmed_subset_small = content_stemmed_subset_small %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_subset_small = cbind(content_stemmed_subset_small, lda.topics)
# colnames(tweets_topics_subset_small) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_subset_small$id = as.numeric(tweets_topics_subset_small$id)
# save.image(file = "subset_small_lda.RData")
# 
# write.csv(tweets_topics_subset_small, "topics_subset_small.csv")
# rm(list = ls())
# 
# 
# 
# 
# 
# 
# 


# ####subset####

load("subset.RData")
k_subset = 4

#Run LDA
lda = LDA(dtm.new, k = k_subset, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 2000))
#Topics found out by the model
lda.topics = as.data.frame(topics(lda))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices = data.frame(dtm$i)
indices$unique = NA
library(data.table)
indices$unique[!duplicated(indices$dtm.i)] = "unique"
indices = indices %>% filter(unique == "unique") %>% select(-unique)
indices = indices$dtm.i
tweets_subset_topics = tweets_subset[indices,]

#Combine
tweets_subset_topics = cbind(tweets_subset_topics, lda.topics)
colnames(tweets_subset_topics) = c("id","tweet_stemmed", "topic")
#convert character of id to numeric
tweets_subset_topics$id = as.numeric(tweets_subset_topics$id)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

save.image(file = "subset_lda4.RData")

write.csv(tweets_subset_topics, "topics4_subset.csv")
rm(list = ls())



# ####shell####

load("shell.RData")
k_shell = 5

#Run LDA
lda = LDA(dtm, k = k_shell, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 2000))
#Topics found out by the model
lda.topics = as.data.frame(topics(lda))
#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
content_stemmed_shell = cbind(content_stemmed_shell, as.data.frame(doc.length))
content_stemmed_shell = content_stemmed_shell %>% filter(doc.length != 0) %>% select(-doc.length)
#Combine
tweets_topics_shell = cbind(content_stemmed_shell, lda.topics)
colnames(tweets_topics_shell) = c("id","company","tweet_stemmed", "topic")
#convert character of id to numeric
tweets_topics_shell$id = as.numeric(tweets_topics_shell$id)
write.csv(tweets_topics_shell, "topics_shell.csv")
save.image(file = "shell_lda.RData")

rm(list = ls())




# ####vw####

# load("vw.RData")
# k_vw = 4
# 
# #Run LDA
# lda = LDA(dtm, k = k_vw, method = 'Gibbs',
#           control = list(nstart = 5, seed = list(1505,99,36,56,88),
#                          best = TRUE, thin = 500, burnin = 4000, iter = 2000))
# #Topics found out by the model
# lda.topics = as.data.frame(topics(lda))
# #Combine content of tweet with Topic Assignment
# #find tweet with less than one non-zero entry
# content_stemmed_vw = cbind(content_stemmed_vw, as.data.frame(doc.length))
# content_stemmed_vw = content_stemmed_vw %>% filter(doc.length != 0) %>% select(-doc.length)
# #Combine
# tweets_topics_vw = cbind(content_stemmed_vw, lda.topics)
# colnames(tweets_topics_vw) = c("id","company","tweet_stemmed", "topic")
# #convert character of id to numeric
# tweets_topics_vw$id = as.numeric(tweets_topics_vw$id)
# write.csv(tweets_topics_vw, "topics_vw.csv")
# save.image(file = "vw_lda.RData")

# rm(list = ls())






