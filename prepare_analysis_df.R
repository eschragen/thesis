library(readr)
library(tidyverse)
library(readxl)
options(scipen=999) #avoid scientific notations (e.g. e+18)

####COMBINE DATAFRAME####
#import all tweets
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df_nonequal_size")
company_info = read_csv("company_info_select.csv")
environmental_claims = read_xlsx("postings_combined.xlsx")
virality_vice = read_csv("df_vice_combined.csv")
user_metadata = read_csv("user_metadata.csv")

user_metadata = user_metadata %>% select(username, followers_count, following_count, tweet_count, user_since)

company_info = company_info %>% select(-c(X1, year, totalGHG))
df_select = df %>% select(id, date, tweet, company,username) %>% left_join(company_info, by = "company") %>% 
  left_join(user_metadata, by = "username") 
#Create variable for environmentally sensitive industries
df_select$industry_brown = FALSE
df_select$industry_brown[df_select$company == "exxonmobil"|df_select$company == "vw"|df_select$company == "shell"] = TRUE
df_select$industry_brown = as.factor(df_select$industry_brown)

#create classes for revenues
#table(df_select$revenues)
df_select$revenues_class = NA
df_select$revenues_class[df_select$revenues <= 33014000000] = "low"
df_select$revenues_class[df_select$revenues <= 92235833400  & df_select$revenues > 33014000000] = "mid"
df_select$revenues_class[df_select$revenues <= 244248134420 & df_select$revenues > 92235833400] = "high"

#compute log of revenues (company size)
df_select = df_select %>% mutate(company_size = log(revenues))

#control for year
df_select$year = as.numeric(format(df_select$date,'%Y'))

environmental_claims = environmental_claims %>% select(-c(day, ratio_env_media))
df_select = df_select %>% left_join(environmental_claims, by = c("company", "date")) 

#change variable name of env_claim_made & substitute 1 with TRUE
df_select = df_select %>% rename("green_ad" = "env_claim_made")
df_select$green_ad[df_select$green_ad == "1"] = "TRUE"
df_select$green_ad[df_select$green_ad == "0"] = "FALSE"

virality_vice = virality_vice %>% select(-c(X1))
df_select = df_select %>% left_join(virality_vice, by = c("company","date")) 

# user_metadata = user_metadata %>% select(-c(id, X1,created_at,today))
# df_select = df_select %>% left_join(user_metadata, by = "username")

#MFT#
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd")
MFT = read_csv("results_NEW.csv")
MFT = MFT %>% group_by(id) %>% mutate(vice_sum = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice),
                                      bias_sum = sum(bias_loyalty, bias_care, bias_fairness, bias_sanctity, bias_authority)) 

max_morality = MFT[,-c(1:8,14:25)]
max_morality$max_morality = colnames(max_morality)[max.col(max_morality, ties.method = "random")]
MFT = cbind(MFT,max_morality[,6])
MFT_select = MFT[,-c(1,3,15,16,18,21,23)]
MFT_select = MFT[,c('id', 'loyalty.vice', 'care.vice','fairness.vice','sanctity.vice', 'authority.vice', 'vice_sum', 'max_morality')]

#include fairness hypothesis
MFT_select$fairness_foundation = FALSE
MFT_select$fairness_foundation[MFT_select$max_morality == "intensity_fairness"] = TRUE

df_select = df_select %>% left_join(MFT_select, by = "id")

#TOPIC MODELING#

topicprob = read.csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/topicprob_df.csv")

topicprob$topic1_dominant = topicprob$topic1
topicprob$topic2_dominant = topicprob$topic2
topicprob$topic3_dominant = topicprob$topic3
topicprob$topic4_dominant = topicprob$topic4

topicprob$topic2_dominant[topicprob$topic == 1] = 0
topicprob$topic3_dominant[topicprob$topic == 1] = 0
topicprob$topic4_dominant[topicprob$topic == 1] = 0

topicprob$topic1_dominant[topicprob$topic == 2] = 0
topicprob$topic3_dominant[topicprob$topic == 2] = 0
topicprob$topic4_dominant[topicprob$topic == 2] = 0

topicprob$topic1_dominant[topicprob$topic == 3] = 0
topicprob$topic2_dominant[topicprob$topic == 3] = 0
topicprob$topic4_dominant[topicprob$topic == 3] = 0

topicprob$topic1_dominant[topicprob$topic == 4] = 0
topicprob$topic2_dominant[topicprob$topic == 4] = 0
topicprob$topic3_dominant[topicprob$topic == 4] = 0

#new threshold topic variable (average 95th quantile of each topic)
#mean(quantile(topicprob$topic1,0.95), quantile(topicprob$topic2,0.95),
#quantile(topicprob$topic3,0.95),quantile(topicprob$topic4,0.95)

# topicprob$topic1_TS = 0
# topicprob$topic1_TS[topicprob$topic1>0.35] = 1
# topicprob$topic2_TS = 0
# topicprob$topic2_TS[topicprob$topic2>0.35] = 1
# topicprob$topic3_TS = 0
# topicprob$topic3_TS[topicprob$topic3>0.35] = 1
# topicprob$topic4_TS = 0
# topicprob$topic4_TS[topicprob$topic4>0.35] = 1

df_select = df_select %>% left_join(topicprob, by = "id") %>% drop_na(topic) %>% select(-X)


setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(df_select, "df_select.csv")



