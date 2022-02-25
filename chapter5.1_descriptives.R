####LIBRARIES####
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(patchwork)
options(scipen=999) #avoid scientific notations
####PREPARE DATA####
data = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")
df = data %>% select(-c(X1,date,tweet,username,employees,total_assets)) 

#compute new output variable (LOG, Factor 100)
df = df %>% mutate(vice_sum_100 = 100*vice_sum,vice_sum_100_log = 100*log(1+vice_sum),
                   fairness.vice_100_log = 100*log(1+fairness.vice),
                   loyalty.vice_100_log = 100*log(1+loyalty.vice),
                   sanctity.vice_100_log = 100*log(1+sanctity.vice),
                   authority.vice_100_log = 100*log(1+authority.vice),
                   care.vice_100_log = 100*log(1+care.vice)) %>% drop_na(vice_sum)
#create factors
df$company = as.factor(df$company)
df$sic = as.factor(df$sic)
df$industry1 = as.factor(df$industry1)
df$industry2 = as.factor(df$industry2)
df$green_ad = as.factor(df$green_ad)
df$max_morality = as.factor(df$max_morality)
df$topic = as.factor(df$topic)
df$industry_brown = as.factor(df$industry_brown)
df$fairness_foundation= as.factor(df$fairness_foundation)

levels(df$max_morality) = c("Authority", "Care", "Fairness", "Loyalty", "Sanctity")


####metric variables####
library(patchwork)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)

df = df %>% drop_na(vice_sum, followers_count)

negative_WOM_plot = df %>% filter(vice_virality <= quantile(vice_virality,0.8)) %>%
ggplot(aes(x = vice_virality, y = vice_sum_100))+ geom_point()+
  geom_smooth() + theme_ipsum() + ylab("Moral Outrage") + xlab("Negative WOM")

followers_count_plot = df %>% filter(followers_count <= quantile(followers_count,0.8)) %>%
ggplot(aes(x = followers_count, y = vice_sum_100))+ geom_point()+
  geom_smooth() + theme_ipsum() + ylab("Moral Outrage") + xlab("#Followers")

following_count_plot = df %>% filter(following_count <= quantile(following_count,0.8)) %>%
ggplot(aes(x = following_count, y = vice_sum_100))+ geom_point()+
  geom_smooth() + theme_ipsum() + ylab("Moral Outrage") + xlab("#Following")

negative_WOM_plot + followers_count_plot + following_count_plot 

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

###visualization categorical####

df_categorical = df %>%
  select(industry_brown, green_ad, topic, max_morality, vice_sum_100)%>% 
  filter(vice_sum_100 <= quantile(vice_sum_100, 1)) 

# industry_plot = df_categorical %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.95)) %>% 
#   ggplot(aes(y = vice_sum_100, x = industry_brown)) + 
#   geom_boxplot(fill = "grey", alpha = 0.8) + xlab("") + ylab("Moral Outrage") +
#   theme_bw() + ggtitle("Brown Industry")
# 
# ad_plot = df_categorical %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.95), !is.na(green_ad)) %>% 
#   ggplot(aes(y = vice_sum_100, x = green_ad)) + 
#   geom_boxplot(fill = "grey", alpha = 0.8) + xlab("") + ylab("Moral Outrage") +
#   theme_bw() + ggtitle("Green Advertising")

# fun_mean <- function(x){
#   y = mean(x,na.rm=T)
#   return(data.frame(y=y,label=round(y,2)))
# }

means_topic = aggregate(vice_sum_100 ~  topic, df_categorical, mean)
topic_plot = df_categorical %>% filter(vice_sum_100 <= quantile(vice_sum_100, 1)) %>% 
  ggplot(aes(y = vice_sum_100, x = topic)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topic") + coord_cartesian(ylim =  c(0, 10))+ 
  stat_summary(fun="mean", size = .3)+ 
  geom_text(size = 3.5, data = means_topic, aes(label = round(vice_sum_100,2), y = vice_sum_100 + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_morality = aggregate(vice_sum_100 ~  max_morality, df_categorical, mean)
foundation_plot = df_categorical %>% 
  ggplot(aes(y = vice_sum_100, x = max_morality)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Dominant Foundation") + coord_cartesian(ylim =  c(0, 10))+
  stat_summary(fun="mean", size = .3)+ 
  geom_text(size = 3.5, data = means_morality, aes(label = round(vice_sum_100,2), y = vice_sum_100 + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

topic_plot + foundation_plot


df_categorical %>% group_by(max_morality) %>% 
  filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>% summarize(mean = mean(vice_sum_100))

df_company = df
levels(df_company$company) = c("Coca Cola", "Exxonmobil", "H&M", "IKEA", "Nestle","Shell","Unilever","Volkswagen")
df_company$industry_brown = factor(df_company$industry_brown, levels = c("TRUE", "FALSE"))

library(forcats)
library(ggplot2)
#boxplot of moral outrage per company
moraloutrage_percompany = df_company %>% filter(vice_sum_100 <= quantile(vice_sum_100, 0.99)) %>%
  ggplot(aes(x = fct_reorder(company,vice_sum_100), y = vice_sum_100, fill = industry_brown)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + xlab("") + ylab("Moral Outrage") +
  theme_bw()  + scale_fill_manual(values=c( "#D2B48C","#b6b6b6")) + labs(fill = "Brown Industry")
# + theme(legend.position="none")
moraloutrage_percompany + coord_flip(ylim = c(0, 10))


