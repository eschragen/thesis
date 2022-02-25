setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
virality_vice = read_csv("df_vice_combined.csv") 
df_select = read_csv("df_select.csv")

MFT_df = df_select %>% select(date, vice_sum, company) %>% filter(company == "nestle")
virality_vice = virality_vice %>% select(date, vice_virality,company) %>% filter(company == "nestle")

test = MFT_df %>% group_by(date) %>% summarise(vice = mean(vice_sum)) %>% left_join(virality_vice, by = "date") %>% drop_na(vice) %>% select(vice, vice_virality)

plot(test)
cor(test$vice,test$vice_virality)
