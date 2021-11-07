#libraries
library(tidyverse)
library(textclean)

setwd("~/GitHub/twint/outputs")

#determine sample size per company (equal sample size --> pick n of smallest: H&M)
n = 4500

####PREPARE COMPANY DATASETS####

#cocacola
cocacola_greenwashing = read_csv("cocacola_greenwashing.csv")
cocacola_hashtags = read_csv("cocacola_hashtags.csv")
cocacola_keywords = read_csv("cocacola_keywords.csv")

cocacola = rbind(cocacola_greenwashing, cocacola_hashtags, cocacola_keywords)

cocacola = cocacola[order(cocacola$user_id, cocacola$date, decreasing=TRUE),]
cocacola = cocacola[!duplicated(cocacola$tweet),]

cocacola = cocacola %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "cocacola") %>%
  sample_n(n)

#shell
shell_greenwashing = read_csv("shell_greenwashing.csv")
shell_hashtags = read_csv("shell_hashtags.csv")
shell_keywords = read_csv("shell_keywords.csv")

shell = rbind(shell_greenwashing, shell_hashtags, shell_keywords)

shell = shell[order(shell$user_id, shell$date, decreasing=TRUE),]
shell = shell[!duplicated(shell$tweet),]

shell = shell %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "shell") %>%
  sample_n(n)


#unilever
unilever_greenwashing = read_csv("unilever_greenwashing.csv")
unilever_hashtags = read_csv("unilever_hashtags.csv")
unilever_keywords = read_csv("unilever_keywords.csv")

unilever = rbind(unilever_greenwashing, unilever_hashtags, unilever_keywords)

unilever = unilever[order(unilever$user_id, unilever$date, decreasing=TRUE),]
unilever = unilever[!duplicated(unilever$tweet),]

unilever = unilever %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "unilever") %>%
  sample_n(n)

#nestle
nestle_greenwashing = read_csv("nestle_greenwashing.csv")
nestle_hashtags = read_csv("nestle_hashtags.csv")
nestle_keywords = read_csv("nestle_keywords.csv")

nestle = rbind(nestle_greenwashing, nestle_hashtags, nestle_keywords)

nestle = nestle[order(nestle$user_id, nestle$date, decreasing=TRUE),]
nestle = nestle[!duplicated(nestle$tweet),]

nestle = nestle %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "nestle") %>%
  sample_n(n)

#mcdonalds
mcdonalds_greenwashing = read_csv("mcdonalds_greenwashing.csv")
mcdonalds_hashtags = read_csv("mcdonalds_hashtags.csv")
mcdonalds_keywords = read_csv("mcdonalds_keywords.csv")

mcdonalds = rbind(mcdonalds_greenwashing, mcdonalds_hashtags, mcdonalds_keywords)

mcdonalds = mcdonalds[order(mcdonalds$user_id, mcdonalds$date, decreasing=TRUE),]
mcdonalds = mcdonalds[!duplicated(mcdonalds$tweet),]

mcdonalds = mcdonalds %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "mcdonalds") %>%
  sample_n(n)

#exxonmobil
exxonmobil_greenwashing = read_csv("exxonmobil_greenwashing.csv")
exxonmobil_hashtags = read_csv("exxonmobil_hashtags.csv")
exxonmobil_keywords = read_csv("exxonmobil_keywords.csv")

exxonmobil = rbind(exxonmobil_greenwashing, exxonmobil_hashtags, exxonmobil_keywords)

exxonmobil = exxonmobil[order(exxonmobil$user_id, exxonmobil$date, decreasing=TRUE),]
exxonmobil = exxonmobil[!duplicated(exxonmobil$tweet),]

exxonmobil = exxonmobil %>%
  distinct %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(company = "exxonmobil") %>%
  sample_n(n)

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

vw = vw %>%
  distinct() %>%
  filter(date > "2015-09-15" & date < "2016-09-15") %>%
  mutate(company = "vw") %>%
  sample_n(n)

#starbucks: 2018-07-08 until 2019-07-08
starbucks_greenwashing = read_csv("starbucks_greenwashing.csv")
starbucks_hashtags = read_csv("starbucks_hashtags.csv")
starbucks_keywords = read_csv("starbucks_keywords.csv")

starbucks = rbind(starbucks_greenwashing, starbucks_hashtags, starbucks_keywords)

starbucks = starbucks[order(starbucks$user_id, starbucks$date, decreasing=TRUE),]
starbucks = starbucks[!duplicated(starbucks$tweet),]

starbucks = starbucks %>%
  distinct() %>%
  filter(date > "2018-07-07" & date < "2019-07-07") %>%
  mutate(company = "starbucks") %>%
  sample_n(n)

#hm: 2020-08-23 until 2021-08-23
hm_greenwashing = read_csv("hm_greenwashing.csv")
hm_hashtags = read_csv("hm_hashtags.csv")
hm_keywords = read_csv("hm_keywords.csv")

hm = rbind(hm_greenwashing, hm_hashtags, hm_keywords)

hm = hm[order(hm$user_id, hm$date, decreasing=TRUE),]
hm = hm[!duplicated(hm$tweet),]

hm = hm %>%
  distinct() %>%
  filter(date > "2020-08-22" & date < "2021-08-22") %>%
  mutate(company = "hm") %>%
  sample_n(n)

#ikea: 2020-08-21 until 2021-08-21
ikea_greenwashing = read_csv("ikea_greenwashing.csv")
ikea_hashtags = read_csv("ikea_hashtags.csv")
ikea_keywords = read_csv("ikea_keywords.csv")

ikea = rbind(ikea_greenwashing, ikea_hashtags, ikea_keywords)

ikea = ikea[order(ikea$user_id, ikea$date, decreasing=TRUE),]
ikea = ikea[!duplicated(ikea$tweet),]

ikea = ikea %>%
  distinct() %>%
  filter(date > "2020-08-20" & date < "2021-08-20") %>%
  mutate(company = "ikea") %>%
  sample_n(n)



####COMBINE IN ONE DATASET####
#TO DO create new df with all companies!
df = rbind(vw, starbucks, hm, ikea, cocacola, shell, unilever, nestle, mcdonalds, exxonmobil)

#exclude non-relevant columns
df = df[order(df$user_id, df$date, decreasing=TRUE),]
df = df[!duplicated(df$tweet),]

df = df %>% select(-c("conversation_id","created_at","timezone","name","place","language","cashtags","near","geo","source","user_rt_id","user_rt","retweet_id","retweet_date","translate","trans_src","trans_dest", "retweet")) %>%
  drop_empty_row()

#extract emojis
df$tweet = gsub(">", "> ", df$tweet)          # Add whitespace after every ">"
df$tweet = gsub("<", " <", df$tweet)          # Add whitespace before every "<"
df$emojis = str_extract(df$tweet, "<[^>]+>")  # Create new column

#save df and tweets
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(df, file = "df")

