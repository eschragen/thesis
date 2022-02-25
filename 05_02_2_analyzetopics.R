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



