library(readr)
library(tidyverse)
library(readxl)
options(scipen=999) #avoid scientific notations (e.g. e+18)

#import data
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df_nonequal_size")
company_info = read_csv("company_info_select.csv")
environmental_claims = read_xlsx("postings_combined.xlsx")
virality_vice = read_csv("df_negativeWOM.csv")
user_metadata = read_csv("user_metadata.csv")
user_metadata = user_metadata %>% select(username, followers_count, following_count, tweet_count, user_since)

#combine data
company_info = company_info %>% select(-c(X1, year, totalGHG))
df_select = df %>% select(id, date, tweet, company,username) %>% left_join(company_info, by = "company") %>% 
  left_join(user_metadata, by = "username") 
#Create variable for brown industries
df_select$industry_brown = FALSE
df_select$industry_brown[df_select$company == "exxonmobil"|df_select$company == "vw"|df_select$company == "shell"] = TRUE
df_select$industry_brown = as.factor(df_select$industry_brown)

environmental_claims = environmental_claims %>% select(-c(day, ratio_env_media))
df_select = df_select %>% left_join(environmental_claims, by = c("company", "date")) 

#change variable name of env_claim_made & substitute 1 with TRUE
df_select = df_select %>% rename("green_ad" = "env_claim_made")
df_select$green_ad[df_select$green_ad == "1"] = "TRUE"
df_select$green_ad[df_select$green_ad == "0"] = "FALSE"

virality_vice = virality_vice %>% select(-c(X1))
df_select = df_select %>% left_join(virality_vice, by = c("company","date")) 

#eMFD Scoring
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd")
MFT = read_csv("results_emfd.csv")
MFT = MFT %>% group_by(id) %>% mutate(vice_sum = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice)) 
max_morality = MFT[,-c(1:8,14:25)]
max_morality$max_morality = colnames(max_morality)[max.col(max_morality, ties.method = "random")]
MFT = cbind(MFT,max_morality[,6])
MFT_select = MFT[,-c(1,3,15,16,18,21,23)]
MFT_select = MFT[,c('id', 'loyalty.vice', 'care.vice','fairness.vice','sanctity.vice', 'authority.vice', 'vice_sum', 'max_morality')]

#Topics
topicprob = read.csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/topicprob_df.csv")
df_select = df_select %>% left_join(topicprob, by = "id") %>% drop_na(topic) %>% select(-X)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(df_select, "df_select.csv")



