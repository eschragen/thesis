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
library(data.table)

#load data
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
load("lda4_topics.RData")

####determine number per company####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints/seperate_topic_moldeing_per_company")
# content_unilever = content_stemmed %>% filter(company == "unilever")
# #create corpus
# tweets_corpus_unilever = VCorpus(VectorSource(content_unilever$tweet_stemmed))
# #Create Document Term Matrix
# dtm_unilever = DocumentTermMatrix(tweets_corpus_unilever)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_unilever$i)
# dtm_unilever.new = dtm_unilever[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_shell = FindTopicsNumber(
#   dtm = dtm_unilever.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result_shell)   #k = 4
# 
# content_cocacola = content_stemmed %>% filter(company == "cocacola")
# #create corpus
# tweets_corpus_cocacola = VCorpus(VectorSource(content_cocacola$tweet_stemmed))
# #Create Document Term Matrix
# dtm_cocacola = DocumentTermMatrix(tweets_corpus_cocacola)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_cocacola$i)
# dtm_cocacola.new = dtm_cocacola[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_cocacola = FindTopicsNumber(
#   dtm = dtm_cocacola.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(result_cocacola) #k = 5
# 
# content_hm = content_stemmed %>% filter(company == "hm")
# #create corpus
# tweets_corpus_hm = VCorpus(VectorSource(content_hm$tweet_stemmed))
# #Create Document Term Matrix
# dtm_hm = DocumentTermMatrix(tweets_corpus_hm)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_hm$i)
# dtm_hm.new = dtm_hm[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_hm = FindTopicsNumber(
#   dtm = dtm_hm.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(result_hm) #k = 4
# 
# content_ikea = content_stemmed %>% filter(company == "ikea")
# #create corpus
# tweets_corpus_ikea = VCorpus(VectorSource(content_ikea$tweet_stemmed))
# #Create Document Term Matrix
# dtm_ikea = DocumentTermMatrix(tweets_corpus_ikea)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_ikea$i)
# dtm_ikea.new = dtm_ikea[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_ikea = FindTopicsNumber(
#   dtm = dtm_ikea.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(result_ikea) #k = 7

# content_nestle = content_stemmed %>% filter(company == "nestle") 
# #create corpus
# tweets_corpus_nestle = VCorpus(VectorSource(content_nestle$tweet_stemmed))
# #Create Document Term Matrix
# dtm_nestle = DocumentTermMatrix(tweets_corpus_nestle)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_nestle$i)
# dtm_nestle.new = dtm_nestle[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_nestle = FindTopicsNumber(
#   dtm = dtm_nestle.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result_nestle) #k = 4

# content_exxonmobil = content_stemmed %>% filter(company == "exxonmobil") %>% sample_n(15000)
# #create corpus
# tweets_corpus_exxonmobil = VCorpus(VectorSource(content_exxonmobil$tweet_stemmed))
# #Create Document Term Matrix
# dtm_exxonmobil = DocumentTermMatrix(tweets_corpus_exxonmobil)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_exxonmobil$i)
# dtm_exxonmobil.new = dtm_exxonmobil[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_exxonmobil = FindTopicsNumber(
#   dtm = dtm_exxonmobil.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result_exxonmobil) #k = 6

# content_shell = content_stemmed %>% filter(company == "shell") %>% sample_n(15000)
# #create corpus
# tweets_corpus_shell = VCorpus(VectorSource(content_shell$tweet_stemmed))
# #Create Document Term Matrix
# dtm_shell = DocumentTermMatrix(tweets_corpus_shell)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_shell$i)
# dtm_shell.new = dtm_shell[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_shell = FindTopicsNumber(
#   dtm = dtm_shell.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result_shell) #k = 5
# 
# content_vw = content_stemmed %>% filter(company == "vw") %>% sample_n(15000)
# #create corpus
# tweets_corpus_vw = VCorpus(VectorSource(content_vw$tweet_stemmed))
# #Create Document Term Matrix
# dtm_vw = DocumentTermMatrix(tweets_corpus_vw)
# #Each row of the input matrix needs to contain at least one non-zero entry
# memory.limit(9999999999)
# ui = unique(dtm_vw$i)
# dtm_vw.new = dtm_vw[ui,]
# #calculate scores for 2-50 topics
# memory.limit(9999999999)
# result_shell = FindTopicsNumber(
#   dtm = dtm_vw.new,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result_shell) #k = 6

####run topic modeling per company####
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/lda4_topics.RData")
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

content_unilever = content_stemmed %>% filter(company == "unilever")
#create corpus
tweets_corpus_unilever = VCorpus(VectorSource(content_unilever$tweet_stemmed))
#Create Document Term Matrix
dtm_unilever = DocumentTermMatrix(tweets_corpus_unilever)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_unilever$i)
dtm_unilever.new = dtm_unilever[ui,]

k_unilever = 4

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_unilever.new, k = k_unilever, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_unilever = data.frame(dtm_unilever$i)
indices_unilever$unique = NA
indices_unilever$unique[!duplicated(indices_unilever$dtm_unilever.i)] = "unique"
indices_unilever = indices_unilever %>% filter(unique == "unique") %>% select(-unique)
indices_unilever = indices_unilever$dtm_unilever.i
tweets_unilever = content_unilever[indices_unilever,1]
#Combine
topics_unilever = cbind(as.data.frame(tweets_unilever), as.data.frame(topics(lda)))
colnames(topics_unilever) = c("id","topic")
write.csv(topics_unilever, "topics_unilever.csv")


content_cocacola = content_stemmed %>% filter(company == "cocacola")
#create corpus
tweets_corpus_cocacola = VCorpus(VectorSource(content_cocacola$tweet_stemmed))
#Create Document Term Matrix
dtm_cocacola = DocumentTermMatrix(tweets_corpus_cocacola)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_cocacola$i)
dtm_cocacola.new = dtm_cocacola[ui,]

k_cocacola = 5

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_cocacola.new, k = k_cocacola, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_cocacola = data.frame(dtm_cocacola$i)
indices_cocacola$unique = NA
indices_cocacola$unique[!duplicated(indices_cocacola$dtm_cocacola.i)] = "unique"
indices_cocacola = indices_cocacola %>% filter(unique == "unique") %>% select(-unique)
indices_cocacola = indices_cocacola$dtm_cocacola.i
tweets_cocacola = content_cocacola[indices_cocacola,1]
#Combine
topics_cocacola = cbind(as.data.frame(tweets_cocacola), as.data.frame(topics(lda)))
colnames(topics_cocacola) = c("id","topic")
write.csv(topics_cocacola, "topics_cocacola.csv")

content_hm = content_stemmed %>% filter(company == "hm")
#create corpus
tweets_corpus_hm = VCorpus(VectorSource(content_hm$tweet_stemmed))
#Create Document Term Matrix
dtm_hm = DocumentTermMatrix(tweets_corpus_hm)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_hm$i)
dtm_hm.new = dtm_hm[ui,]

k_hm = 4

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_hm.new, k = k_hm, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_hm = data.frame(dtm_hm$i)
indices_hm$unique = NA
indices_hm$unique[!duplicated(indices_hm$dtm_hm.i)] = "unique"
indices_hm = indices_hm %>% filter(unique == "unique") %>% select(-unique)
indices_hm = indices_hm$dtm_hm.i
tweets_hm = content_hm[indices_hm,1]
#Combine
topics_hm = cbind(as.data.frame(tweets_hm), as.data.frame(topics(lda)))
colnames(topics_hm) = c("id","topic")
write.csv(topics_hm, "topics_hm.csv")

content_ikea = content_stemmed %>% filter(company == "ikea")
#create corpus
tweets_corpus_ikea = VCorpus(VectorSource(content_ikea$tweet_stemmed))
#Create Document Term Matrix
dtm_ikea = DocumentTermMatrix(tweets_corpus_ikea)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_ikea$i)
dtm_ikea.new = dtm_ikea[ui,]

k_ikea = 7

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_ikea.new, k = k_ikea, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_ikea = data.frame(dtm_ikea$i)
indices_ikea$unique = NA
indices_ikea$unique[!duplicated(indices_ikea$dtm_ikea.i)] = "unique"
indices_ikea = indices_ikea %>% filter(unique == "unique") %>% select(-unique)
indices_ikea = indices_ikea$dtm_ikea.i
tweets_ikea = content_ikea[indices_ikea,1]
#Combine
topics_ikea = cbind(as.data.frame(tweets_ikea), as.data.frame(topics(lda)))
colnames(topics_ikea) = c("id","topic")
write.csv(topics_ikea, "topics_ikea.csv")


content_nestle = content_stemmed %>% filter(company == "nestle")
#create corpus
tweets_corpus_nestle = VCorpus(VectorSource(content_nestle$tweet_stemmed))
#Create Document Term Matrix
dtm_nestle = DocumentTermMatrix(tweets_corpus_nestle)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_nestle$i)
dtm_nestle.new = dtm_nestle[ui,]

k_nestle = 4

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_nestle.new, k = k_nestle, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_nestle = data.frame(dtm_nestle$i)
indices_nestle$unique = NA
indices_nestle$unique[!duplicated(indices_nestle$dtm_nestle.i)] = "unique"
indices_nestle = indices_nestle %>% filter(unique == "unique") %>% select(-unique)
indices_nestle = indices_nestle$dtm_nestle.i
tweets_nestle = content_nestle[indices_nestle,1]
#Combine
topics_nestle = cbind(as.data.frame(tweets_nestle), as.data.frame(topics(lda)))
colnames(topics_nestle) = c("id","topic")
write.csv(topics_nestle, "topics_nestle.csv")

rm(list=(ls()[ls()!="content_stemmed"]))

content_exxonmobil = content_stemmed %>% filter(company == "exxonmobil")
#create corpus
tweets_corpus_exxonmobil = VCorpus(VectorSource(content_exxonmobil$tweet_stemmed))
#Create Document Term Matrix
dtm_exxonmobil = DocumentTermMatrix(tweets_corpus_exxonmobil)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_exxonmobil$i)
dtm_exxonmobil.new = dtm_exxonmobil[ui,]

k_exxonmobil = 6

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_exxonmobil.new, k = k_exxonmobil, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_exxonmobil = data.frame(dtm_exxonmobil$i)
indices_exxonmobil$unique = NA
indices_exxonmobil$unique[!duplicated(indices_exxonmobil$dtm_exxonmobil.i)] = "unique"
indices_exxonmobil = indices_exxonmobil %>% filter(unique == "unique") %>% select(-unique)
indices_exxonmobil = indices_exxonmobil$dtm_exxonmobil.i
tweets_exxonmobil = content_exxonmobil[indices_exxonmobil,1]
#Combine
topics_exxonmobil = cbind(as.data.frame(tweets_exxonmobil), as.data.frame(topics(lda)))
colnames(topics_exxonmobil) = c("id","topic")
write.csv(topics_exxonmobil, "topics_exxonmobil.csv")

rm(list=(ls()[ls()!="content_stemmed"]))

content_shell = content_stemmed %>% filter(company == "shell")
#create corpus
tweets_corpus_shell = VCorpus(VectorSource(content_shell$tweet_stemmed))
#Create Document Term Matrix
dtm_shell = DocumentTermMatrix(tweets_corpus_shell)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_shell$i)
dtm_shell.new = dtm_shell[ui,]

k_shell = 5

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_shell.new, k = k_shell, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_shell = data.frame(dtm_shell$i)
indices_shell$unique = NA
indices_shell$unique[!duplicated(indices_shell$dtm_shell.i)] = "unique"
indices_shell = indices_shell %>% filter(unique == "unique") %>% select(-unique)
indices_shell = indices_shell$dtm_shell.i
tweets_shell = content_shell[indices_shell,1]
#Combine
topics_shell = cbind(as.data.frame(tweets_shell), as.data.frame(topics(lda)))
colnames(topics_shell) = c("id","topic")
write.csv(topics_shell, "topics_shell.csv")

rm(list=(ls()[ls()!="content_stemmed"]))

content_vw = content_stemmed %>% filter(company == "vw")
#create corpus
tweets_corpus_vw = VCorpus(VectorSource(content_vw$tweet_stemmed))
#Create Document Term Matrix
dtm_vw = DocumentTermMatrix(tweets_corpus_vw)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_vw$i)
dtm_vw.new = dtm_vw[ui,]

k_vw = 6

#Run LDA
memory.limit(9999999999)
lda = LDA(dtm_vw.new, k = k_vw, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices_vw = data.frame(dtm_vw$i)
indices_vw$unique = NA
indices_vw$unique[!duplicated(indices_vw$dtm_vw.i)] = "unique"
indices_vw = indices_vw %>% filter(unique == "unique") %>% select(-unique)
indices_vw = indices_vw$dtm_vw.i
tweets_vw = content_vw[indices_vw,1]
#Combine
topics_vw = cbind(as.data.frame(tweets_vw), as.data.frame(topics(lda)))
colnames(topics_vw) = c("id","topic")
write.csv(topics_vw, "topics_vw.csv")


####save topic modeling results for manual labeling####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

content_stemmed = read_csv("content_stemmed.csv")
content_unilever = content_stemmed %>% filter(company == "unilever")
#create corpus
tweets_corpus_unilever = VCorpus(VectorSource(content_unilever$tweet_stemmed))
#Create Document Term Matrix
dtm_unilever = DocumentTermMatrix(tweets_corpus_unilever)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_unilever$i)
dtm_unilever.new = dtm_unilever[ui,]

k_unilever = 4

#Run LDA
memory.limit(9999999999)
lda_unilever = LDA(dtm_unilever.new, k = k_unilever, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

top30terms_unilever = as.data.frame(terms(lda_unilever,30))
writexl::write_xlsx(top30terms_unilever, "top30terms_unilever.xlsx")


content_stemmed = read_csv("content_stemmed.csv")
content_cocacola = content_stemmed %>% filter(company == "cocacola")
#create corpus
tweets_corpus_cocacola = VCorpus(VectorSource(content_cocacola$tweet_stemmed))
#Create Document Term Matrix
dtm_cocacola = DocumentTermMatrix(tweets_corpus_cocacola)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_cocacola$i)
dtm_cocacola.new = dtm_cocacola[ui,]

k_cocacola = 5

#Run LDA
memory.limit(9999999999)
lda_cocacola = LDA(dtm_cocacola.new, k = k_cocacola, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))
top30terms_cocacola = as.data.frame(terms(lda_cocacola,30))
writexl::write_xlsx(top30terms_cocacola, "top30terms_cocacola.xlsx")


content_stemmed = read_csv("content_stemmed.csv")
content_hm = content_stemmed %>% filter(company == "hm")
#create corpus
tweets_corpus_hm = VCorpus(VectorSource(content_hm$tweet_stemmed))
#Create Document Term Matrix
dtm_hm = DocumentTermMatrix(tweets_corpus_hm)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_hm$i)
dtm_hm.new = dtm_hm[ui,]

k_hm = 4

#Run LDA
memory.limit(9999999999)
lda_hm = LDA(dtm_hm.new, k = k_hm, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))
top30terms_hm = as.data.frame(terms(lda_hm,30))
writexl::write_xlsx(top30terms_hm, "top30terms_hm.xlsx")

content_stemmed = read_csv("content_stemmed.csv")
content_ikea = content_stemmed %>% filter(company == "ikea")
#create corpus
tweets_corpus_ikea = VCorpus(VectorSource(content_ikea$tweet_stemmed))
#Create Document Term Matrix
dtm_ikea = DocumentTermMatrix(tweets_corpus_ikea)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_ikea$i)
dtm_ikea.new = dtm_ikea[ui,]

k_ikea = 7

#Run LDA
memory.limit(9999999999)
lda_ikea = LDA(dtm_ikea.new, k = k_ikea, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))
top30terms_ikea = as.data.frame(terms(lda_ikea,30))
writexl::write_xlsx(top30terms_ikea, "top30terms_ikea.xlsx")


content_stemmed = read_csv("content_stemmed.csv")
content_nestle = content_stemmed %>% filter(company == "nestle")
#create corpus
tweets_corpus_nestle = VCorpus(VectorSource(content_nestle$tweet_stemmed))
#Create Document Term Matrix
dtm_nestle = DocumentTermMatrix(tweets_corpus_nestle)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_nestle$i)
dtm_nestle.new = dtm_nestle[ui,]

k_nestle = 4

#Run LDA
memory.limit(9999999999)
lda_nestle = LDA(dtm_nestle.new, k = k_nestle, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))
top30terms_nestle = as.data.frame(terms(lda_nestle,30))
writexl::write_xlsx(top30terms_nestle, "top30terms_nestle.xlsx")


content_stemmed = read_csv("content_stemmed.csv")
content_exxonmobil = content_stemmed %>% filter(company == "exxonmobil")
#create corpus
tweets_corpus_exxonmobil = VCorpus(VectorSource(content_exxonmobil$tweet_stemmed))
#Create Document Term Matrix
dtm_exxonmobil = DocumentTermMatrix(tweets_corpus_exxonmobil)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_exxonmobil$i)
dtm_exxonmobil.new = dtm_exxonmobil[ui,]

k_exxonmobil = 6

#Run LDA
memory.limit(9999999999)
lda_exxonmobil = LDA(dtm_exxonmobil.new, k = k_exxonmobil, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))
top30terms_exxonmobil = as.data.frame(terms(lda_exxonmobil,30))
writexl::write_xlsx(top30terms_exxonmobil, "top30terms_exxonmobil.xlsx")

content_stemmed = read_csv("content_stemmed.csv")
content_shell = content_stemmed %>% filter(company == "shell")
#create corpus
tweets_corpus_shell = VCorpus(VectorSource(content_shell$tweet_stemmed))
#Create Document Term Matrix
dtm_shell = DocumentTermMatrix(tweets_corpus_shell)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_shell$i)
dtm_shell.new = dtm_shell[ui,]

k_shell = 5

#Run LDA
memory.limit(9999999999)
lda_shell = LDA(dtm_shell.new, k = k_shell, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

top30terms_shell = as.data.frame(terms(lda_shell,30))
writexl::write_xlsx(top30terms_shell, "top30terms_shell.xlsx")




content_stemmed = read_csv("content_stemmed.csv")
content_vw = content_stemmed %>% filter(company == "vw")
#create corpus
tweets_corpus_vw = VCorpus(VectorSource(content_vw$tweet_stemmed))
#Create Document Term Matrix
dtm_vw = DocumentTermMatrix(tweets_corpus_vw)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)
ui = unique(dtm_vw$i)
dtm_vw.new = dtm_vw[ui,]

k_vw = 6

#Run LDA
memory.limit(9999999999)
lda_vw = LDA(dtm_vw.new, k = k_vw, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

top30terms_vw = as.data.frame(terms(lda_vw,30))
writexl::write_xlsx(top30terms_vw, "top30terms_vw.xlsx")
