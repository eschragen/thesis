#libraries
library(tidyverse)
library(textclean)
library(data.table)

setwd("~/GitHub/twint/outputs")
years = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/year2015_2021.csv")

####PREPARE COMPANY DATASETS####

#cocacola
cocacola_greenwashing = read_csv("cocacola_greenwashing.csv")
cocacola_hashtags = read_csv("cocacola_hashtags.csv")
cocacola_keywords = read_csv("cocacola_keywords.csv")

cocacola = rbind(cocacola_greenwashing, cocacola_hashtags, cocacola_keywords)

cocacola = cocacola[order(cocacola$user_id, cocacola$date, decreasing=TRUE),]
cocacola = cocacola[!duplicated(cocacola$tweet),]

virality_cocacola = cocacola %>% select(date, id) %>% group_by(date) %>% count()
virality_cocacola = years %>% left_join(virality_cocacola, by = "date") 
virality_cocacola[is.na(virality_cocacola)] = 0
setDT(virality_cocacola)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_cocacola$virality[i] = sum(virality_cocacola$n[1:i])
  i = i +1
}

cocacola = cocacola %>% left_join(virality_cocacola, by = "date") %>% select(-n) %>% 
  mutate(company = "cocacola") %>% filter(date > "2020-11-01" & date < "2021-11-01")  

#shell
shell_greenwashing = read_csv("shell_greenwashing.csv")
shell_hashtags = read_csv("shell_hashtags.csv")
shell_keywords = read_csv("shell_keywords.csv")

shell = rbind(shell_greenwashing, shell_hashtags, shell_keywords)

shell = shell[order(shell$user_id, shell$date, decreasing=TRUE),]
shell = shell[!duplicated(shell$tweet),]

virality_shell = shell %>% select(date, id) %>% group_by(date) %>% count()
virality_shell = years %>% left_join(virality_shell, by = "date") 
virality_shell[is.na(virality_shell)] = 0
setDT(virality_shell)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_shell$virality[i] = sum(virality_shell$n[1:i])
  i = i +1
}

shell = shell %>% left_join(virality_shell, by = "date") %>% select(-n) %>% 
  mutate(company = "shell") %>% filter(date > "2020-11-01" & date < "2021-11-01")  


#unilever
unilever_greenwashing = read_csv("unilever_greenwashing.csv")
unilever_hashtags = read_csv("unilever_hashtags.csv")
unilever_keywords = read_csv("unilever_keywords.csv")

unilever = rbind(unilever_greenwashing, unilever_hashtags, unilever_keywords)

unilever = unilever[order(unilever$user_id, unilever$date, decreasing=TRUE),]
unilever = unilever[!duplicated(unilever$tweet),]

virality_unilever = unilever %>% select(date, id) %>% group_by(date) %>% count()
virality_unilever = years %>% left_join(virality_unilever, by = "date") 
virality_unilever[is.na(virality_unilever)] = 0
setDT(virality_unilever)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_unilever$virality[i] = sum(virality_unilever$n[1:i])
  i = i +1
}

unilever = unilever %>% left_join(virality_unilever, by = "date") %>% select(-n)  %>%  
  mutate(company = "unilever") %>% filter(date > "2020-11-01" & date < "2021-11-01") 

#nestle
nestle_greenwashing = read_csv("nestle_greenwashing.csv")
nestle_hashtags = read_csv("nestle_hashtags.csv")
nestle_keywords = read_csv("nestle_keywords.csv")

nestle = rbind(nestle_greenwashing, nestle_hashtags, nestle_keywords)

nestle = nestle[order(nestle$user_id, nestle$date, decreasing=TRUE),]
nestle = nestle[!duplicated(nestle$tweet),]

virality_nestle = nestle %>% select(date, id) %>% group_by(date) %>% count()
virality_nestle = years %>% left_join(virality_nestle, by = "date") 
virality_nestle[is.na(virality_nestle)] = 0
setDT(virality_nestle)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_nestle$virality[i] = sum(virality_nestle$n[1:i])
  i = i +1
}

nestle = nestle %>% left_join(virality_nestle, by = "date") %>% select(-n)  %>% 
  mutate(company = "nestle") %>% filter(date > "2020-11-01" & date < "2021-11-01") 


#exxonmobil
exxonmobil_greenwashing = read_csv("exxonmobil_greenwashing.csv")
exxonmobil_hashtags = read_csv("exxonmobil_hashtags.csv")
exxonmobil_keywords = read_csv("exxonmobil_keywords.csv")

exxonmobil = rbind(exxonmobil_greenwashing, exxonmobil_hashtags, exxonmobil_keywords)

exxonmobil = exxonmobil[order(exxonmobil$user_id, exxonmobil$date, decreasing=TRUE),]
exxonmobil = exxonmobil[!duplicated(exxonmobil$tweet),]

virality_exxonmobil = exxonmobil %>% select(date, id) %>% group_by(date) %>% count()
virality_exxonmobil = years %>% left_join(virality_exxonmobil, by = "date") 
virality_exxonmobil[is.na(virality_exxonmobil)] = 0
setDT(virality_exxonmobil)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_exxonmobil$virality[i] = sum(virality_exxonmobil$n[1:i])
  i = i +1
}

exxonmobil = exxonmobil %>% left_join(virality_exxonmobil, by = "date") %>% select(-n) %>% 
  mutate(company = "exxonmobil") %>% filter(date > "2020-11-01" & date < "2021-11-01")  

#vw: 2015-09-16 until 2016-09-16
vw_greenwashing = read_csv("vw_greenwashing.csv")
vw_hashtags = read_csv("vw_hashtags.csv")
vw_keywords = read_csv("vw_keywords.csv")
volkswagen_greenwashing = read_csv("volkswagen_greenwashing.csv")
volkswagen_hashtags = read_csv("volkswagen_hashtags.csv")
volkswagen_keywords = read_csv("volkswagen_keywords.csv")

vw = rbind(vw_greenwashing, vw_hashtags, vw_keywords, volkswagen_greenwashing, volkswagen_hashtags, volkswagen_keywords)

vw = vw[order(vw$user_id, vw$date, decreasing=TRUE),]
vw = vw[!duplicated(vw$tweet),]

virality_vw = vw %>% select(date, id) %>% group_by(date) %>% count()
virality_vw = years %>% left_join(virality_vw, by = "date") 
virality_vw[is.na(virality_vw)] = 0
setDT(virality_vw)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_vw$virality[i] = sum(virality_vw$n[1:i])
  i = i +1
}

vw = vw %>% left_join(virality_vw, by = "date") %>% select(-n)  %>% 
  mutate(company = "vw") %>% filter(date > "2015-09-16" & date < "2016-09-16") 

#hm: 2020-08-23 until 2021-08-23
hm_greenwashing = read_csv("hm_greenwashing.csv")
hm_hashtags = read_csv("hm_hashtags.csv")
hm_keywords = read_csv("hm_keywords.csv")

hm = rbind(hm_greenwashing, hm_hashtags, hm_keywords)

hm = hm[order(hm$user_id, hm$date, decreasing=TRUE),]
hm = hm[!duplicated(hm$tweet),]

virality_hm = hm %>% select(date, id) %>% group_by(date) %>% count()
virality_hm = years %>% left_join(virality_hm, by = "date") 
virality_hm[is.na(virality_hm)] = 0
setDT(virality_hm)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_hm$virality[i] = sum(virality_hm$n[1:i])
  i = i +1
}

hm = hm %>% left_join(virality_hm, by = "date") %>% select(-n) %>% 
  mutate(company = "hm") %>% filter(date > "2020-08-23" & date < "2021-08-23")  

#ikea: 2020-08-21 until 2021-08-21
ikea_greenwashing = read_csv("ikea_greenwashing.csv")
ikea_hashtags = read_csv("ikea_hashtags.csv")
ikea_keywords = read_csv("ikea_keywords.csv")

ikea = rbind(ikea_greenwashing, ikea_hashtags, ikea_keywords)

ikea = ikea[order(ikea$user_id, ikea$date, decreasing=TRUE),]
ikea = ikea[!duplicated(ikea$tweet),]

virality_ikea = ikea %>% select(date, id) %>% group_by(date) %>% count()
virality_ikea = years %>% left_join(virality_ikea, by = "date") 
virality_ikea[is.na(virality_ikea)] = 0
setDT(virality_ikea)[, virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  virality_ikea$virality[i] = sum(virality_ikea$n[1:i])
  i = i +1
}

ikea = ikea %>% left_join(virality_ikea, by = "date") %>% select(-n)  %>% 
  mutate(company = "ikea") %>% filter(date > "2020-08-21" & date < "2021-08-21") 

####COMBINE IN ONE DATASET####
df = rbind(vw, hm, ikea, cocacola, shell, unilever, nestle, exxonmobil)

#exclude non-relevant columns
df = df[order(df$user_id, df$date, decreasing=TRUE),]
df = df[!duplicated(df$tweet),]

df = df %>% select(-c("conversation_id","created_at","timezone","name","place","language","cashtags","near","geo","source","user_rt_id","user_rt","retweet_id","retweet_date","translate","trans_src","trans_dest", "retweet")) %>%
  drop_empty_row()

#save df with smaller subset of VW
average_amount_tweets = df %>% filter(company != "vw") %>% group_by(company) %>% count()
mean(average_amount_tweets$n)

df_subsetvw = df %>% filter(company == "vw") %>% sample_n(mean(average_amount_tweets$n))
df_withoutvw = df %>% filter(company != "vw") 
df_nonequal_size_subsetVW = rbind(df_subsetvw,df_withoutvw)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(df_nonequal_size_subsetVW, file = "df_nonequal_size_subsetVW.csv")


