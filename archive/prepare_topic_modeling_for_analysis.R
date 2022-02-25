library(tidyverse)

#import topics
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")
topics_nestle = read_csv("topics_nestle.csv")
topics_ikea = read_csv("topics_ikea.csv")
topics_exxonmobil = read_csv("topics_exxonmobil.csv")
topics_cocacola = read_csv("topics_cocacola.csv")
topics_unilever = read_csv("topics_unilever.csv")
topics_hm = read_csv("topics_hm.csv")
topics_shell = read_csv("topics_shell.csv")
topics_vw = read_csv("topics_vw.csv")
topics_subset = read_csv("topics_subset.csv")

topic_per_company = rbind(topics_nestle, topics_ikea, topics_exxonmobil,topics_cocacola,topics_unilever, topics_hm, topics_shell, topics_vw)
topic_per_company = topic_per_company[,c(3,6)]
colnames(topic_per_company) = c("id", "topic")

topics_subset = topics_subset[,c(4,7)]
colnames(topics_subset) = c("id", "topic_total_subset")

topic_per_company = topic_per_company %>% left_join(topics_subset, by = "id")

write.csv(topic_per_company, "topic_per_company")


