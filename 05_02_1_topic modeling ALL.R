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


# ####PREPARE DTM####
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/tweets_decreasedVW_stemmed.RData")

#remove strings that contain "I just voted for this Shell"
#get content of stemmed corpus
content_stemmed = as.data.frame(sapply(tweets_corpus_stemmed, function(x){x$content}))     #transponse (t) when you apply lemmatization
content_stemmed = as.data.frame(cbind(tweets_subset, content_stemmed))
colnames(content_stemmed) = c("id","tweet", "tweet_stemmed")

df_company = df %>% select(id, company)

remove = "I just voted for this Shell"

content_stemmed$remove = NA

for (i in 1:length(content_stemmed$id)){
  content_stemmed$remove[i] = grepl(remove, content_stemmed$tweet[i])
}

content_stemmed = content_stemmed %>% filter(remove == FALSE) %>% select(-remove)

content_stemmed$tweet_stemmed = gsub("volkswagen", "", content_stemmed$tweet_stemmed) #Remove "volkswagen"

content_stemmed_company = content_stemmed %>% left_join(df_company, by = "id")

content_stemmed_company_shell = content_stemmed_company %>% filter(company == "shell")
necklace = "necklace"
earring = "earring"
green = "green"
turtle = "turtle"

content_stemmed_company_shell$necklace = NA
content_stemmed_company_shell$earring = NA
content_stemmed_company_shell$green = NA
content_stemmed_company_shell$turtle = NA

for (i in 1:length(content_stemmed_company_shell$id)){
  content_stemmed_company_shell$necklace[i] = grepl(necklace, content_stemmed_company_shell$tweet[i], ignore.case = TRUE)
  content_stemmed_company_shell$earring[i] = grepl(earring, content_stemmed_company_shell$tweet[i], ignore.case = TRUE)
  content_stemmed_company_shell$green[i] = grepl(green, content_stemmed_company_shell$tweet[i], ignore.case = TRUE)
  content_stemmed_company_shell$turtle[i] = grepl(turtle, content_stemmed_company_shell$tweet[i], ignore.case = TRUE)
  }

content_stemmed_company_shell_clean = content_stemmed_company_shell %>% filter(necklace == FALSE)  
content_stemmed_company_shell_clean = content_stemmed_company_shell_clean %>% filter(earring == FALSE)  
content_stemmed_company_shell_clean = content_stemmed_company_shell_clean %>% filter(green == FALSE)  
content_stemmed_company_shell_clean = content_stemmed_company_shell_clean %>% filter(turtle == FALSE)  
content_stemmed_company_shell_clean = content_stemmed_company_shell_clean %>% select(-c(necklace, earring, green, turtle))

content_stemmed_company_others = content_stemmed_company %>% filter(company != "shell")

content_stemmed = rbind(content_stemmed_company_shell_clean,content_stemmed_company_others )

#create corpus
tweets_corpus_stemmed = VCorpus(VectorSource(content_stemmed$tweet_stemmed))

#Create Document Term Matrix
dtm = DocumentTermMatrix(tweets_corpus_stemmed)
#Each row of the input matrix needs to contain at least one non-zero entry
memory.limit(9999999999)

ui = unique(dtm$i)
dtm.new = dtm[ui,]

####DETERMINE NUMBER OF TOPICS####
#calculate scores for 2-50 topics
memory.limit(9999999999)
result_subset = FindTopicsNumber(
  dtm = dtm.new,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12345),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result_subset)

save.image(file = "findtopicnumber_shell_cleaned.RData")


# memory.limit(9999999999)
# # 
# mod_log_lik = numeric(50)
# mod_perplexity = numeric(50)
# for (i in 2:50) {
#    mod = LDA(dtm.new, k = i, method = "Gibbs",
#              control = list(alpha = 0.5, iter = 100, seed=12345, thin =1))
#    mod_log_lik[i] = logLik(mod)
#    mod_perplexity[i] = perplexity(mod, dtm.new)
# }
# beepr::beep(8)
# perplexity_df = as.data.frame(mod_perplexity)
# perplexity_df$topics = as.numeric(rownames(perplexity_df))
# 
# save.image(file = "subset_perplexity.RData")

# ggplot(perplexity_df, aes(x = topics, y = mod_perplexity)) +
#   geom_line() + geom_point() + ggtitle("Model Perplexity") + theme_ipsum() +
#   scale_x_continuous(name="# Topics", limits=c(2, 10)) +
#   scale_y_continuous(name="Perplexity")

#rm(list = ls())




# ####RUN TOPIC MODELING####
k = 4

#Run LDA
lda = LDA(dtm.new, k = k, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

beepr::beep(3)

save.image(file = "lda4_final.RData")

# #Topics found out by the model
lda.topics = as.data.frame(topics(lda))
# 
#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices = data.frame(dtm$i)
indices$unique = NA
library(data.table)
indices$unique[!duplicated(indices$dtm.i)] = "unique"
indices = indices %>% filter(unique == "unique") %>% select(-unique)
indices = indices$dtm.i
tweets_subset_topics = tweets_subset[indices,]
# 
#Combine
tweets_subset_topics = cbind(tweets_subset_topics, lda.topics)
colnames(tweets_subset_topics) = c("id","tweet_stemmed", "topic")

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

save.image(file = "lda4_topics.RData")

write.csv(tweets_subset_topics, "topics4.csv")

####ANALYZE TOPICS####
#load data
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/lda4_topics.RData")

## Word frequencies & Distributions
#Top 10 terms or words under each topic
top30terms = as.matrix(terms(lda,30))
top30terms

#Beta probabilities for each word
topicprob = as.matrix(lda@gamma)
word_topicprob = tidytext::tidy(lda, matrix = "beta")

top_terms_per_topic = word_topicprob %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)

# top_terms_per_topic %>% 
#   mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   tidytext::scale_y_reordered()

top_terms_per_topic$topic[top_terms_per_topic$topic == 1] = "Topic 1: Resource Utilization"
top_terms_per_topic$topic[top_terms_per_topic$topic == 2] = "Topic 2: Corporate Greed"
top_terms_per_topic$topic[top_terms_per_topic$topic == 3] = "Topic 3: Call to Action"
top_terms_per_topic$topic[top_terms_per_topic$topic == 4] = "Topic 4: Future Impact"

top_terms_per_topic %>% 
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  xlab("Beta Value") + ylab("Term")+
  scale_x_continuous(breaks = seq(0, 0.06, 0.02)) +
  facet_wrap(~ topic, scale = "free_y") +
  theme(strip.text.x = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))



####MFT SCORING####
MFT = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd/results_NEW.csv")

# tweets_topics_subset = tweets_topics_subset[,c(2,3,4,5)]
# colnames(tweets_topics_subset) = c("id","tweet_stemmed","company","topic")
# 
# # ##for small subset
# # tweets_topics_subset = tweets_topics_subset[,c(3,4,5,6)]
# # colnames(tweets_topics_subset) = c("id","company","tweet_stemmed","topic")
# 

tweets_subset_topics$topic = as.factor(tweets_subset_topics$topic)

MFT_topics_subset = tweets_subset_topics %>% left_join(MFT, by = "id") %>% drop_na(tweet)
MFT_topics_subset = MFT_topics_subset %>% group_by(id) %>% mutate(sum_vice = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice),
                                                                  sum_virtue = sum(loyalty.virtue, care.virtue, fairness.virtue, sanctity.virtue, authority.virtue),
                                                                  sum_bias = sum(bias_loyalty, bias_care, bias_fairness, bias_sanctity, bias_authority))

vice_mean_per_topic = MFT_topics_subset %>% group_by(topic) %>% summarise(mean = mean(sum_vice), sd = sd(sum_vice),
                                                                          min = min(sum_vice), max = max(sum_vice),
                                                                          median = median(sum_vice))
vice_mean_per_topic

MFT_topics_subset %>% group_by(topic) %>% summarise(fairness_mean = mean(fairness.vice), loyalty_mean = mean(loyalty.vice),
                                                    care_mean = mean(care.vice),sanctity_mean = mean(sanctity.vice),authority_mean = mean(authority.vice))

# virtue_mean_per_topic = MFT_topics_subset %>% group_by(topic) %>% summarise(mean = mean(sum_virtue))
# virtue_mean_per_topic

MFT_topics_subset_gather = MFT_topics_subset %>% gather(key="measure", "value",7:26)
MFT_vice_subset = MFT_topics_subset_gather %>% filter(grepl('.vice',measure))

MFT_vice_subset$measure[MFT_vice_subset$measure == "authority.vice"] = "Subversion"
MFT_vice_subset$measure[MFT_vice_subset$measure == "care.vice"] = "Harm"
MFT_vice_subset$measure[MFT_vice_subset$measure == "fairness.vice"] = "Cheating"
MFT_vice_subset$measure[MFT_vice_subset$measure == "loyalty.vice"] = "Betrayal"
MFT_vice_subset$measure[MFT_vice_subset$measure == "sanctity.vice"] = "Degradation"

MFT_vice_subset %>% filter(measure != "sum_vice") %>%
ggplot(aes(fill = measure, y = value, x=topic))  + 
      geom_bar(position="dodge", stat="identity") + theme_ipsum() +
      ylab("Value") +xlab("Topic") + labs(fill = "Moral Foundation:\n Vice Dimension") +
      theme(panel.grid.major.x = element_blank())

# svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\Foundationpertopic.svg",bg = "transparent")
# print(
#   ggplot(MFT_vice_subset, aes(fill = measure, y = value, x=topic))  + 
#     geom_bar(position="dodge", stat="identity") + theme_ipsum() +
#     ylab("Value") +xlab("Topic") + labs(fill = "Moral Foundation:\n Vice Dimension") +
#     theme(panel.grid.major.x = element_blank())
# )
# dev.off()

#####EXPORTS TO COMPARE TOPIC RESULTS####

##load data
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")
#load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/lda4_topics.RData")

#Top 30 terms or words under each topic
top30terms = as.data.frame(terms(lda,30))
writexl::write_xlsx(top30terms, "top30terms4.xlsx")

#Beta probabilities for each word
topicprob = as.data.frame(lda@gamma)
topicprob = cbind(tweets_subset_topics, topicprob)
topicprob_df = topicprob %>% select(-tweet_stemmed)
colnames(topicprob_df) = c("id","topic","topic1","topic2","topic3","topic4")
# setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")
# write.csv(topicprob_df, "topicprob_df.csv")

df_topic = df %>% select(id, tweet)
# df_topic$tweet = iconv(df_topic$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
topicprob = topicprob %>% left_join(df_topic, by = "id") 
topicprob$tweet = gsub("\r", "", topicprob$tweet) #Remove tabs
topicprob$tweet_stemmed = gsub("\r", "", topicprob$tweet_stemmed) #Remove tabs

write.csv(topicprob, "topicprob4.csv")
writexl::write_xlsx(topicprob, "topicprob4.xlsx")


#topicprob with MFT
MFT_prob_per_tweet = MFT_topics_subset %>% select(id, sum_vice)
topicprob_MFT = topicprob %>% left_join(MFT_prob_per_tweet, by = "id")
writexl::write_xlsx(topicprob_MFT, "topicprob_MFT.xlsx")

