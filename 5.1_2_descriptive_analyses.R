####LIBRARIES####
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(patchwork)
options(scipen=999) 
library(forcats)
library(ggplot2)

####PREPARE DATA####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
data = read_csv("df_select.csv")
df = data %>% select(-c(X1,date,tweet,username,employees,total_assets)) 

#compute new output variable (LOG, Factor 100)
df = df %>% mutate(vice_sum_100 = 100*vice_sum) %>% drop_na(vice_sum)

#create factors
df$green_ad = as.factor(df$green_ad)
df$max_morality = as.factor(df$max_morality)
df$topic = as.factor(df$topic)
df$industry_brown = as.factor(df$industry_brown)

levels(df$max_morality) = c("Authority", "Care", "Fairness", "Loyalty", "Sanctity")

df = df %>% drop_na(vice_sum, followers_count)

#exclude outliers of metric variables
df = df %>% filter(following_count <= quantile(following_count, 0.95, na.rm = TRUE),
                   followers_count <= quantile(followers_count, 0.95, na.rm = TRUE),
                   vice_virality <= quantile(vice_virality, 0.95, na.rm = TRUE))

####metric variables####
df %>% 
  summarize(mean_WOM = mean(vice_virality),
                 median_WOM = median(vice_virality),
                 sd_WOM = sd(vice_virality),
                 min_WOM = min(vice_virality),
                 max_WOM = max(vice_virality))

df %>% 
  summarize(mean_followers = mean(followers_count),
                 median_followers = median(followers_count),
                 sd_followers = sd(followers_count),
                 min_followers = min(followers_count),
                 max_followers = max(followers_count))

df %>%     
  summarize(mean_following = mean(following_count),
                 median_following = median(following_count),
                 sd_following = sd(following_count),
                 min_following = min(following_count),
                 max_following = max(following_count)) 

df %>%     
  summarize(mean_moral_outrage = mean(vice_sum_100),
            median_moral_outrage = median(vice_sum_100),
            sd_moral_outrage = sd(vice_sum_100),
            min_moral_outrage = min(vice_sum_100),
            max_moral_outrage = max(vice_sum_100)) 

df %>% group_by(company) %>%      
  summarize(mean_moral_outrage = mean(vice_sum_100),
            median_moral_outrage = median(vice_sum_100),
            sd_moral_outrage = sd(vice_sum_100),
            min_moral_outrage = min(vice_sum_100),
            max_moral_outrage = max(vice_sum_100)) 

####categorical variables####
df %>% group_by(industry_brown) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(green_ad)) %>% group_by(green_ad) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(topic)) %>% group_by(topic) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(max_morality)) %>% group_by(max_morality) %>% count() %>% mutate(ratio = n/nrow(df))

#examine green advertising strategy per company
library(readxl)
postings_combined = read_excel("postings_combined.xlsx")
postings_combined %>% group_by(company) %>% count(env_claim_made) %>% mutate(ratio = n/365)
postings_boolean = postings_combined %>% group_by(company) %>% count(env_claim_made) %>% mutate(ratio = round(100*(n/365),0))
postings_boolean$Advertising = NA
postings_boolean$Advertising[postings_boolean$env_claim_made == 1] = "Green Advertising"
postings_boolean$Advertising[postings_boolean$env_claim_made == 0] = "No/General Advertising"
postings_boolean$company = as.factor(postings_boolean$company)
levels(postings_boolean$company) =  c("Coca-Cola", "ExxonMobil", "H&M", "IKEA", "Nestlé","Shell","Unilever","VW")
postings_boolean %>%
    ggplot(aes(x="", y=n, fill=Advertising)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme(legend.position="none") +
    theme_void() +
    xlab("Count") +
    theme(
      legend.position="right",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 16),
      plot.title = element_text(size=16), 
      text=element_text(size=16,  family="sans")
      
    ) + facet_wrap(~company, nrow = 2) +
  geom_label(aes(label = paste(ratio,"%",sep = " ")),
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(values = c("darkgrey","lightgrey")) +
  theme(legend.position ="bottom",legend.title = element_blank())

###visualization moral outrage per company####
df_company = df
df_company$company = as.factor(df_company$company)
levels(df_company$company) =  c("Coca-Cola", "ExxonMobil", "H&M", "IKEA", "Nestlé","Shell","Unilever","Volkswagen")
df_company$industry_brown = factor(df_company$industry_brown, levels = c("TRUE", "FALSE"))

#boxplot of moral outrage per company
moraloutrage_percompany = df_company %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>%
  ggplot(aes(x = fct_reorder(company,vice_sum_100), y = vice_sum_100, fill = industry_brown)) + 
  geom_boxplot(outlier.shape = NA, width = .7) + xlab("") + ylab("Moral Outrage") + 
  theme_bw()  + theme(text = element_text(size = 16)) + scale_fill_manual(values=c("#D2B48C","#b6b6b6")) + labs(fill = "Brown Industry")
  
moraloutrage_percompany + coord_flip(ylim = c(0, 10))

####visualization moral outrage per topic & foundation####

df_categorical = df %>%
  select(topic, max_morality, vice_sum_100)

means_topic = aggregate(vice_sum_100 ~  topic, df_categorical, mean)
topic_plot = df_categorical %>% filter(vice_sum_100 <= quantile(vice_sum_100, 1)) %>% 
  ggplot(aes(y = vice_sum_100, x = topic)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topic") + coord_cartesian(ylim =  c(0, 10))+ 
  stat_summary(fun="mean", size = .3)+ 
  geom_text(size = 4.3, data = means_topic, aes(label = round(vice_sum_100,2), y = vice_sum_100 +.3))+
  theme(plot.title = element_text(size=20),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

means_morality = aggregate(vice_sum_100 ~  max_morality, df_categorical, mean)
foundation_plot = df_categorical %>% 
  ggplot(aes(y = vice_sum_100, x = max_morality)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Dominant Foundation") + coord_cartesian(ylim =  c(0, 10))+
  stat_summary(fun="mean", size = .3)+ 
  geom_text(size = 4.3, data = means_morality, aes(label = round(vice_sum_100,2), y = vice_sum_100 +.3))+
  theme(plot.title = element_text(size=20),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

topic_plot + foundation_plot 
