library(readxl)
library(tidyverse)
library(data.table)
options(scipen=999) #avoid scientific notations (e.g. e+18)

####import data####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
claims = read_excel("company_profiles.xlsx")
video = read_excel("company_profiles_video.xlsx")
video = video %>% select(id, real_id)
df = claims %>% left_join(video, by = "id") %>% select(real_id, company) %>% filter(!is.na(environment)) 
claims_env = df %>% select(real_id) %>% drop_na
claims_env_vec = claims_env$real_id
#length(claims_env_vec)

#devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE)

library(academictwitteR)
#get_bearer()
#vignette("academictwitteR-intro")

#TWITTER_BEARER=AAAAAAAAAAAAAAAAAAAAANlzVAEAAAAA8Pc5SBGAXjyNCgCdVWakBNQ2KEw%3DVT1f7rmvdKo43fqWH8xLfEU0TDBdDk3yGt1uk5yU9GRJEnsbSQ

####LOOP####

for (i in 1:length(claims_env_vec)) {
tweets = get_all_tweets(
  conversation_id =  claims_env_vec[i],
  start_tweets = "2015-01-01T00:00:00Z",
  end_tweets = "2021-12-17T00:00:00Z",
  data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""),
  n = Inf
)
}


total_df_cocacola = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation1")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation1")$conversation_id)   

for (i in 2:2) {
  total_df_cocacola = rbind(total_df_cocacola, 
                            (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_cocacola = cbind(total_df_cocacola, "cocacola")

total_df_exxonmobil = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation3")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation3")$conversation_id)   

for (i in 4:49) {
  total_df_exxonmobil = rbind(total_df_exxonmobil, 
                            (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_exxonmobil = cbind(total_df_exxonmobil, "exxonmobil")

total_df_hm = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation50")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation50")$conversation_id)   

for (i in 51:75) {
  total_df_hm = rbind(total_df_hm, 
                              (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_hm = cbind(total_df_hm, "hm")

total_df_ikea = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation76")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation76")$conversation_id)   

for (i in 77:133) {
  total_df_ikea = rbind(total_df_ikea, 
                      (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_ikea = cbind(total_df_ikea, "ikea")

total_df_nestle = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation134")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation134")$conversation_id)   

for (i in 135:350) {
  total_df_nestle = rbind(total_df_nestle, 
                        (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}

total_df_nestle = cbind(total_df_nestle, "nestle")


total_df_shell = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation351")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation351")$conversation_id)   

for (i in 352:418) {
  total_df_shell = rbind(total_df_shell, 
                          (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_shell = cbind(total_df_shell, "shell")


total_df_unilever = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation419")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation419")$conversation_id)   


for (i in 420:665) {
  total_df_unilever = rbind(total_df_unilever, 
                         (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/Desktop/conversations/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_unilever = cbind(total_df_unilever, "unilever")

df_date = as.data.frame(rbind(total_df_cocacola, total_df_exxonmobil,total_df_hm, total_df_ikea,total_df_nestle,total_df_shell,total_df_unilever))


colnames(df_date) = c("date","conversation_id","company")
df_date$date = as.Date(df_date$date, format = "%Y-%m-%d")
df_date = df_date %>% group_by(conversation_id, date, company) %>% count()
#Loop: Count Click Path of each Session
df_date$day = NA
count = 1
currentSession = " "
for (i in 1:length(df_date$conversation_id)) {
  if(df_date$conversation_id[i] == currentSession) {
    df_date$day[i] = count
    count = count + 1
  } else{
    count = 1
    df_date$day[i]= count
    currentSession = df_date$conversation_id[i]
    count = count + 1 
  }
}



library(ggplot2)
library(hrbrthemes)
library(viridis)

df_date %>%
  ggplot(aes(x = day, y = n, color = conversation_id))+
  #geom_area(alpha=0.5) +
  geom_line() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum() +
  ylab("Engagement")+
  xlab("Day") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=10), 
    text=element_text(size=8,  family="sans")) + 
  scale_x_continuous(limits = c(0,10)) +
facet_wrap(~company, nrow = 2, scales = "free_y")

df_mean = df_date %>% group_by(company, day) %>%
  summarise(average_engagement = mean(n)) 
df_mean %>%
  ggplot(aes(x = day, y = average_engagement, color = company))+
  #geom_area(alpha=0.5) +
  geom_line()




