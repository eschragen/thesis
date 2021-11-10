#Apply Scoring without POS tagging 
source("~/GitHub/thesis/04_clean_tweets.R")
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")

#Export lemmatized tweets
content_lem = as.data.frame(sapply(tweets_corpus_stemmed, function(x){x$content}))     #transponse (t) when you apply lemmatization
content_lem = cbind(tweets_subset[,1], content_lem)
colnames(content_lem) = c("id", "tweet")
write.csv(content_lem, file = "content_lem.csv", row.names = FALSE)

#Export non-lemmatized tweets
write.csv(content, file = "content_nonlem.csv", row.names = FALSE)




