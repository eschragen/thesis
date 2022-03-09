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
library(patchwork)

#####PREPARE DTM####
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints/tweets_decreasedVW_stemmed.RData")

#get content of stemmed corpus
content_stemmed = as.data.frame(sapply(tweets_corpus_stemmed, function(x){x$content}))     
content_stemmed = as.data.frame(cbind(tweets_subset, content_stemmed))
colnames(content_stemmed) = c("id","tweet", "tweet_stemmed")

df_company = df %>% select(id, company)

#####remove tweets that are manually identified as not relevant to greenwashing allegations####
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

####PREPARE TOPIC MODELING#####
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

FindTopicsNumber_plot(result_subset) #best k = 4

save.image(file = "findtopicnumber_shell_cleaned.RData")

#VISUALIZE
load("findtopicnumber_shell_cleaned.RData")
min_max = function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
result_subset_norm = as.data.frame(lapply(result_subset[,-1], min_max))
result_subset_norm = cbind(result_subset[,1],result_subset_norm)
colnames(result_subset_norm)[1] = "topics"
findk_melt = reshape2::melt(result_subset_norm, id.var = "topics")
findk_melt$metric[findk_melt$variable == "CaoJuan2009" | findk_melt$variable == "Arun2010"] = "Minimize"
findk_melt$metric[findk_melt$variable == "Deveaud2014" | findk_melt$variable == "Griffiths2004"] = "Maximize"

#MINIMIZE: CaoJuan2009, Arun2010
findk_plot = findk_melt %>%
ggplot(aes(x=topics, y=value,shape = variable)) + geom_line() + geom_point(size = 2)+
  theme_bw()  + theme(text = element_text(size = 20),panel.grid.major.x = element_line(size = .8,color="darkgrey"),
                      panel.grid.minor.x = element_line(size = .8,color="darkgrey"),
                      panel.grid.major.y = element_blank() ,
                      panel.grid.minor.y = element_blank() )+
  xlab("#Topics") + ylab("Value") + labs(shape = "Metrics")+
  facet_grid(rows = vars(metric))+
  scale_x_continuous(breaks = seq(2, 50, by = 2),expand = c(0, 0))
findk_plot


####RUN TOPIC MODELING####
k = 4

#Run LDA
lda = LDA(dtm.new, k = k, method = 'Gibbs',
          control = list(nstart = 5, seed = list(1505,99,36,56,88),
                         best = TRUE, thin = 500, burnin = 4000, iter = 1000))

#Topics found out by the model
lda.topics = as.data.frame(topics(lda))
 
#Combine content of tweet with Topic Assignment
#find tweet with less than one non-zero entry
indices = data.frame(dtm$i)
indices$unique = NA
indices$unique[!duplicated(indices$dtm.i)] = "unique"
indices = indices %>% filter(unique == "unique") %>% select(-unique)
indices = indices$dtm.i
tweets_subset_topics = tweets_subset[indices,]

#Combine
tweets_subset_topics = cbind(tweets_subset_topics, lda.topics)
colnames(tweets_subset_topics) = c("id","tweet_stemmed", "topic")

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
save.image(file = "lda4_topics.RData")

#Top 30 terms or words under each topic
load("lda4_topics.RData")
top30terms = as.data.frame(terms(lda,30))
writexl::write_xlsx(top30terms, "top30terms4.xlsx")

#Assess tweets with high topic probabilities
df_topic = df %>% select(id, tweet)
topicprob = topicprob %>% left_join(df_topic, by = "id") 
topicprob$tweet = gsub("\r", "", topicprob$tweet) #Remove tabs
topicprob$tweet_stemmed = gsub("\r", "", topicprob$tweet_stemmed) #Remove tabs
write.csv(topicprob, "topicprob4.csv")

#Prepare df for regression analysis
topicprob = as.data.frame(lda@gamma)
topicprob = cbind(tweets_subset_topics, topicprob)
topicprob_df = topicprob %>% select(-tweet_stemmed)
colnames(topicprob_df) = c("id","topic","topic1","topic2","topic3","topic4")
write.csv(topicprob_df, "topicprob_df.csv")

#Visualize 4 Topics
load("lda4_topics.RData")
#Beta probabilities for each word
word_topicprob = tidytext::tidy(lda, matrix = "beta")

top_terms_per_topic = word_topicprob %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)

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


