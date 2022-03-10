library(readxl)
library(tidyverse)
library(data.table)
options(scipen=999) 
library(academictwitteR)
library(ggplot2)
library(hrbrthemes)
library(viridis)

####Import data####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
claims = read_excel("company_profiles.xlsx")
df = claims %>% select(id, company) %>% drop_na(id) 
claims_vec = df$id

#####Collect tweets by conversation_id####
for (i in 1:length(claims_vec)) {
  tweets = get_all_tweets(
    conversation_id =  claims_vec[i],
    start_tweets = "2015-01-01T00:00:00Z",
    end_tweets = "2021-12-17T00:00:00Z",
    data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""),
    n = Inf
  )
}

####Prepare df per company & combine####
# # find out number of postings per company to seperate correctly 
#table(df$company)
# # cocacola exxonmobil   hm       ikea     nestle      shell   unilever 
# # 147         85        118       177        514        129        675 
total_df_cocacola = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1")$conversation_id)   

for (i in 2:147) {
  total_df_cocacola = rbind(total_df_cocacola, 
                            (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_cocacola = cbind(total_df_cocacola, "cocacola")

total_df_exxonmobil = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation148")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation148")$conversation_id)   

for (i in 149:232) {
  total_df_exxonmobil = rbind(total_df_exxonmobil, 
                              (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_exxonmobil = cbind(total_df_exxonmobil, "exxonmobil")

total_df_hm = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation233")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation233")$conversation_id)   

for (i in 234:350) {
  total_df_hm = rbind(total_df_hm, 
                      (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_hm = cbind(total_df_hm, "hm")

total_df_ikea = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation351")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation351")$conversation_id)   

for (i in 352:527) {
  total_df_ikea = rbind(total_df_ikea, 
                        (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_ikea = cbind(total_df_ikea, "ikea")

total_df_nestle = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation528")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation528")$conversation_id)   

for (i in 529:1041) {
  total_df_nestle = rbind(total_df_nestle, 
                          (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}

total_df_nestle = cbind(total_df_nestle, "nestle")


total_df_shell = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1042")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1042")$conversation_id)   

for (i in 1043:1170) {
  total_df_shell = rbind(total_df_shell, 
                         (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_shell = cbind(total_df_shell, "shell")


total_df_unilever = cbind(bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1171")$created_at, bind_tweets(data_path = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation1171")$conversation_id)   


for (i in 1172:1845) {
  total_df_unilever = rbind(total_df_unilever, 
                            (cbind(bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$created_at ,bind_tweets(data_path = paste("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/twint_scraping_outputs/conversations_all_claims/company_conversation",i,"/",sep=""))$conversation_id)))
}
total_df_unilever = cbind(total_df_unilever, "unilever")

#combine all companies
df_date = as.data.frame(rbind(total_df_cocacola, total_df_exxonmobil,total_df_hm, total_df_ikea,total_df_nestle,total_df_shell,total_df_unilever))
colnames(df_date) = c("date","conversation_id","company")
df_date$date = as.Date(df_date$date, format = "%Y-%m-%d")
df_date = df_date %>% group_by(conversation_id, date, company) %>% count()

#Loop: Count clicks per day
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

write.csv(df_date,"df_date.csv")


####Visualize####
df_date = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints/df_date.csv")

#Cumulated percentage per Conversation
require(dplyr)
df_cumulated = df_date %>% group_by(conversation_id) %>% mutate(csum = cumsum(n),
                                                                csum_percentage = 100*cumsum(n)/sum(n))
df_cumulated$conversation_id =as.factor(df_cumulated$conversation_id)

df_cumulated %>% filter(conversation_id != "1438484481036492805") %>% #filter out extreme case (detected visually)
  ggplot(aes(x=day, y = csum_percentage, color = conversation_id)) + geom_line() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ylab("Cumulated % of Comments")+
  xlab("Day") + 
  geom_vline(xintercept = 7, linetype = "dashed",color = "red", size = 1.3) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  geom_text(x = 13, y = 10,label = "Considered\nTimeframe:\n7 Days",color="red",size = 5.5, check_overlap = T)+  
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 16),
    plot.title = element_text(size=16), 
    text = element_text(size = 18),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.x =  element_line(size = .1,color="lightgrey"),
    panel.grid.minor.x =  element_line(size = .1,color="lightgrey"),
    panel.border = element_rect(colour = "black",fill=NA,size = 1)) +
  scale_color_grey(start = 0.8, end = 0.2) 


#boxplot: identify outliers --> 95 quantile at 42 days
df_cumulated %>% ggplot(aes(day)) + geom_boxplot()
quantile(df_cumulated$day, 0.95)

#Subset: Cumulated Percentage >90% <91%
df_cumulated %>% filter(csum_percentage >= 90 & csum_percentage <= 91 & day <=42)%>%
  ggplot(aes(day)) + geom_boxplot()

#Summary of days when 90% of comments is reached: On average 11, to increase probability of visibility, we continue with 7 days
df_cumulated %>% filter(csum_percentage >= 90 & csum_percentage < 91 & day <=42) %>% summary()


