####LIBRARIES####
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(patchwork)
options(scipen=999) #avoid scientific notations
library(forcats)
library(ggplot2)
####PREPARE DATA####
data = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")
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
df %>% filter(vice_virality <= quantile(vice_virality, 0.9)) %>%
  summarize(mean_WOM = mean(vice_virality),
                 median_WOM = median(vice_virality),
                 sd_WOM = sd(vice_virality),
                 min_WOM = min(vice_virality),
                 max_WOM = max(vice_virality))

df %>% filter(followers_count <= quantile(followers_count, 0.9)) %>%
        summarize(mean_followers = mean(followers_count),
                 median_followers = median(followers_count),
                 sd_followers = sd(followers_count),
                 min_followers = min(followers_count),
                 max_followers = max(followers_count))

df %>% filter(following_count <= quantile(following_count, 0.9)) %>%     
          summarize(mean_following = mean(following_count),
                 median_following = median(following_count),
                 sd_following = sd(following_count),
                 min_following = min(following_count),
                 max_following = max(following_count)) 


df %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>%     
  summarize(mean_following = mean(vice_sum_100),
            median_following = median(vice_sum_100),
            sd_following = sd(vice_sum_100),
            min_following = min(vice_sum_100),
            max_following = max(vice_sum_100)) 


df %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>% group_by(company) %>%    
  summarize(mean_following = mean(vice_sum_100),
            median_following = median(vice_sum_100),
            sd_following = sd(vice_sum_100),
            min_following = min(vice_sum_100),
            max_following = max(vice_sum_100)) 

####categorical variables####
df %>% group_by(industry_brown) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(green_ad)) %>% group_by(green_ad) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(topic)) %>% group_by(topic) %>% count() %>% mutate(ratio = n/nrow(df))

df %>% filter(!is.na(max_morality)) %>% group_by(max_morality) %>% count() %>% mutate(ratio = n/nrow(df))

###visualization moral outrage per company####
df_company = df
levels(df_company$company) = c("Coca Cola", "Exxonmobil", "H&M", "IKEA", "Nestle","Shell","Unilever","Volkswagen")
df_company$industry_brown = factor(df_company$industry_brown, levels = c("TRUE", "FALSE"))


#boxplot of moral outrage per company
moraloutrage_percompany = df_company %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>%
  ggplot(aes(x = fct_reorder(company,vice_sum_100), y = vice_sum_100, fill = industry_brown)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + xlab("") + ylab("Moral Outrage") +
  theme_bw()  + scale_fill_manual(values=c( "#D2B48C","#b6b6b6")) + labs(fill = "Brown Industry")

moraloutrage_percompany + coord_flip(ylim = c(0, 10))


