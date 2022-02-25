#source tweets_corpus_stemmed
#load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/content_stemmed.RData")
#get content of stemmed corpus
content_stemmed = as.data.frame(sapply(tweets_corpus_stemmed, function(x){x$content}))     #transponse (t) when you apply lemmatization
content_stemmed = as.data.frame(cbind(tweets_subset[,1], content_stemmed[,1]))
colnames(content_stemmed) = c("id", "tweet_stemmed")
content_stemmed = content_stemmed %>% left_join(df, by = "id") %>% select(id, company, tweet_stemmed)

#divide stemmed content per company
# shell
content_stemmed_shell = content_stemmed %>% filter(company == "shell")
# exxonmobil
content_stemmed_exxonmobil = content_stemmed %>% filter(company == "exxonmobil")
# cocacola
content_stemmed_cocacola = content_stemmed %>% filter(company == "cocacola")
# hm
content_stemmed_hm = content_stemmed %>% filter(company == "hm")
# ikea
content_stemmed_ikea = content_stemmed %>% filter(company == "ikea" & !grepl('actually',tweet_stemmed))
# unilever
content_stemmed_unilever = content_stemmed %>% filter(company == "unilever")
# vw
content_stemmed_vw = content_stemmed %>% filter(company == "vw")
# nestle
content_stemmed_nestle = content_stemmed %>% filter(company == "nestle")

# # equal sizes per company
# content_stemmed_subset = content_stemmed %>% group_by(company) %>% sample_n(4400)

#export dataframes
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")
write.csv(content_stemmed_nestle, "content_stemmed_nestle.csv")
write.csv(content_stemmed_vw, "content_stemmed_vw.csv")
write.csv(content_stemmed_unilever, "content_stemmed_unilever.csv")
write.csv(content_stemmed_ikea, "content_stemmed_ikea.csv")
write.csv(content_stemmed_hm, "content_stemmed_hm.csv")
write.csv(content_stemmed_cocacola, "content_stemmed_cocacola.csv")
write.csv(content_stemmed_exxonmobil, "content_stemmed_exxonmobil.csv")
write.csv(content_stemmed_shell, "content_stemmed_shell.csv")
#write.csv(content_stemmed_subset, "content_stemmed_subset.csv")


# create new subset without keyword "actually" in IKEA df
content_stemmed_nestle = read.csv("content_stemmed_nestle.csv")
content_stemmed_vw = read.csv("content_stemmed_vw.csv")
content_stemmed_unilever = read.csv("content_stemmed_unilever.csv")

content_stemmed_ikea = read.csv("content_stemmed_ikea.csv")
content_stemmed_ikea = content_stemmed_ikea %>% filter(!grepl('actually',tweet_stemmed))

content_stemmed_hm = read.csv("content_stemmed_hm.csv")
content_stemmed_cocacola = read.csv("content_stemmed_cocacola.csv")
content_stemmed_exxonmobil = read.csv("content_stemmed_exxonmobil.csv")
content_stemmed_shell = read.csv("content_stemmed_shell.csv")

content_stemmed_subset = rbind(content_stemmed_nestle,content_stemmed_vw,content_stemmed_unilever, content_stemmed_ikea, 
                               content_stemmed_hm, content_stemmed_cocacola, content_stemmed_exxonmobil, content_stemmed_shell)
content_stemmed_subset = content_stemmed_subset %>% group_by(company) %>% sample_n(4400)
write.csv(content_stemmed_subset, "content_stemmed_subset.csv")


# equal sizes per company SMALL
content_stemmed_subset_small = content_stemmed_subset %>% group_by(company) %>% sample_n(1000)
write.csv(content_stemmed_subset_small, "content_stemmed_subset_small.csv")


