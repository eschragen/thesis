library(tidyverse)
library(ggplot2)
library(tidyr)
MFT = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd/results_withstopwords.csv")
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")

# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/nestle_lda.RData")
# nestle_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/exxonmobil_lda.RData")
# exxonmobil_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/hm_lda.RData")
# hm_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/unilever_lda.RData")
# unilever_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/cocacola_lda.RData")
# cocacola_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/ikea_lda.RData")
# ikea_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/subset_small_lda.RData")
# subset_small_lda = lda
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/subset_lda.RData")
subset_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/vw_lda.RData")
# vw_lda = lda
# load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/shell_lda.RData")
# shell_lda = lda

####VISUALIZATIONS####
# #Top 10 terms or words under each topic
top30terms = as.matrix(terms(nestle_lda,30))
top30terms

# #Beta probabilities for each word
topicprob = as.matrix(nestle_lda@gamma)
word_topicprob = tidytext::tidy(lda, matrix = "beta")
# 
top_terms_per_topic = word_topicprob %>%
   group_by(topic) %>%
   slice_max(beta, n = 20) %>%
   ungroup() %>%
   arrange(topic, -beta)
top_terms_per_topic
top_terms_per_topic %>%
   mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
   ggplot(aes(beta, term, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic, scales = "free") +
   tidytext::scale_y_reordered()


## create wordcloud
library(wordcloud)
set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
tweets_corpus_stemmed_subset = tm::VCorpus(tm::VectorSource(content_stemmed_subset$tweet_stemmed))
wordcloud(tweets_corpus_stemmed_subset, min.freq = 50, scale = c(4, 0.2) , random.order = TRUE, col = palet)


####MFT SCORING####

tweets_topics_subset = tweets_topics_subset[,c(2,3,4,5)]
colnames(tweets_topics_subset) = c("id","tweet_stemmed","company","topic")

# ##for small subset
# tweets_topics_subset = tweets_topics_subset[,c(3,4,5,6)]
# colnames(tweets_topics_subset) = c("id","company","tweet_stemmed","topic")


tweets_topics_subset$topic = as.factor(tweets_topics_subset$topic)

MFT_topics_subset = tweets_topics_subset %>% left_join(MFT, by = "id") %>% drop_na(tweet)
MFT_topics_subset = MFT_topics_subset %>% group_by(id) %>% mutate(sum_vice = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice),
                                                              sum_virtue = sum(loyalty.virtue, care.virtue, fairness.virtue, sanctity.virtue, authority.virtue),
                                                              sum_bias = sum(bias_loyalty, bias_care, bias_fairness, bias_sanctity, bias_authority))

vice_mean_per_topic = MFT_topics_subset %>% group_by(topic) %>% summarise(mean = mean(sum_vice))
vice_mean_per_topic

virtue_mean_per_topic = MFT_topics_subset %>% group_by(topic) %>% summarise(mean = mean(sum_virtue))
virtue_mean_per_topic

MFT_topics_subset_gather = MFT_topics_subset %>% gather(key="measure", "value",7:26)
MFT_vice_subset = MFT_topics_subset_gather %>% filter(grepl('vice',measure))

MFT_vice_subset$measure[MFT_vice_subset$measure == "authority.vice"] = "Subversion"
MFT_vice_subset$measure[MFT_vice_subset$measure == "care.vice"] = "Harm"
MFT_vice_subset$measure[MFT_vice_subset$measure == "fairness.vice"] = "Cheating"
MFT_vice_subset$measure[MFT_vice_subset$measure == "loyalty.vice"] = "Betrayal"
MFT_vice_subset$measure[MFT_vice_subset$measure == "sanctity.vice"] = "Degradation"


svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\Foundationpertopic.svg",bg = "transparent")
print(
   ggplot(MFT_vice_subset, aes(fill = measure, y = value, x=topic))  + 
      geom_bar(position="dodge", stat="identity") + theme_ipsum() +
      ylab("Value") +xlab("Topic") + labs(fill = "Moral Foundation:\n Vice Dimension") +
      theme(panel.grid.major.x = element_blank())
   )
dev.off()


####NEU###
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
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/lda4_topics.RData")

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


