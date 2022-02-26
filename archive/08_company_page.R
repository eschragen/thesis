library(readr)
library(tidyverse)
library(tm)
library(textclean)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(textstem)
library(writexl)
options(scipen=999) #avoid scientific notations (e.g. e+18)

setwd("~/GitHub/twint/outputs/profile_tweets")

####import data & filter out replies####
profile_cocacola = read_csv("profile_cocacola.csv")
profile_cocacola$is_answer = startsWith(profile_cocacola$tweet, "@")
profile_cocacola = profile_cocacola %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "cocacola") %>% filter(date > "2020-10-01" & date < "2021-11-01") 

profile_exxonmobil = read_csv("profile_exxonmobil.csv")
profile_exxonmobil$is_answer = startsWith(profile_exxonmobil$tweet, "@")
profile_exxonmobil = profile_exxonmobil %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "exxonmobil") %>% filter(date > "2020-10-01" & date < "2021-11-01")

profile_hm = read_csv("profile_hm.csv")
profile_hm$is_answer = startsWith(profile_hm$tweet, "@")
profile_hm = profile_hm %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "hm") %>% filter(date > "2020-07-23" & date < "2021-08-23")

profile_ikea = read_csv("profile_ikea.csv")
profile_ikea$is_answer = startsWith(profile_ikea$tweet, "@")
profile_ikea = profile_ikea %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "ikea") %>% filter(date > "2020-07-21" & date < "2021-08-21")

profile_nestle = read_csv("profile_nestle.csv")
profile_nestle$is_answer = startsWith(profile_nestle$tweet, "@")
profile_nestle = profile_nestle %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "nestle") %>% filter(date > "2020-10-01" & date < "2021-11-01")

profile_shell = read_csv("profile_shell.csv")
profile_shell$is_answer = startsWith(profile_shell$tweet, "@")
profile_shell = profile_shell %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "shell") %>% filter(date > "2020-10-01" & date < "2021-11-01")

profile_unilever = read_csv("profile_unilever.csv")
profile_unilever$is_answer = startsWith(profile_unilever$tweet, "@")
profile_unilever = profile_unilever %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "unilever") %>% filter(date > "2020-10-01" & date < "2021-11-01")

profile_vw = read_xlsx("profile_vw.xlsx")
profile_vw$is_answer = startsWith(profile_vw$tweet, "@")
profile_vw = profile_vw %>% filter(is_answer == FALSE) %>% select(id, date, tweet, video, link)%>% 
  mutate(company = "vw") %>% filter(date > "2015-08-16" & date < "2016-09-16")

company_profiles = rbind(profile_cocacola, profile_exxonmobil, profile_hm, profile_ikea, profile_nestle, profile_shell, profile_unilever, profile_vw)
company_profiles$real_id = sub(".*status/", "", company_profiles$link)  
company_profiles = company_profiles %>% select(-link)

#save df
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write_xlsx(x = company_profiles, path = "company_profiles.xlsx", col_names = TRUE)



