library(readr)
library(tidyverse)
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")

#prepare data for scoring
df = read_csv("df_nonequal_size_subsetVW.csv")
df_MFT = df %>% select(id, tweet)
df_MFT$tweet = iconv(df_MFT$tweet, to = "ASCII", sub = " ") #Remove non-ASCII characters
df_MFT$tweet = gsub("[ |\t]{2,}", "", df_MFT$tweet) #Remove tabs
df_MFT$tweet = gsub("[ |\n]{2,}", "", df_MFT$tweet) #Remove tabs
df_MFT$tweet = gsub("[ |\r]{2,}", "", df_MFT$tweet) #Remove tabs
df_MFT$tweet =   sub("\r", "", df_MFT$tweet, fixed = TRUE)
df_MFT[df_MFT==""] = NA
df_MFT = df_MFT[complete.cases(df_MFT),]
write.csv(df_MFT, file = "content_scoring.csv")

# Run eMFD Scoring via cmd ("4.1_2_emfdcode")

# Import scored data
cmd = read_csv("results_emfd.csv")
cmd = cmd[,-1]

#analyze bias (sum of vice & virtue)
cmd_emfd = cmd %>% select(id, tweet, bias_loyalty, bias_fairness, bias_sanctity, bias_authority, bias_care) %>% group_by(id) %>%
  mutate(MFT_score = sum(bias_loyalty, bias_fairness, bias_sanctity, bias_authority, bias_care))
df_emfd = df %>% left_join(cmd_emfd, by = "id") 

#Visualize: Bias per company
library(ggplot2)
MFTcompany = df_emfd %>% select(id, company, MFT_score) %>% ggplot(aes(x = factor(company), y = MFT_score)) + 
  geom_bar(stat = "summary", fun = "mean")
MFTcompany


