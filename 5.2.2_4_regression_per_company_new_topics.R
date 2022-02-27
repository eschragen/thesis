####LIBRARIES####
library(tidyverse)
library(jtools)
library(Rcpp)
library(mctest)
library(ggplot2)
library(stats)
options(scipen=999) 

####PREPARE DATA####
data = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")
df = data %>% select(-c(X1,date,tweet,username,employees,total_assets)) 

#compute new output variable (* 100)
df = df %>% mutate(vice_sum_100 = 100*vice_sum) %>% drop_na(vice_sum, followers_count)

#create factors
df$green_ad = as.factor(df$green_ad)
df$max_morality = as.factor(df$max_morality)
df$topic = as.factor(df$topic)
df$industry_brown = as.factor(df$industry_brown)

#exclude outliers of metric variables
df = df %>% filter(following_count <= quantile(following_count, 0.95, na.rm = TRUE),
                   followers_count <= quantile(followers_count, 0.95, na.rm = TRUE),
                   vice_virality <= quantile(vice_virality, 0.95, na.rm = TRUE))

#normalize metric features
df_numeric = df %>% select(-c(id, company, sic, industry1,industry2,industry_brown, revenues_class,
                              green_ad, max_morality, topic, year,
                              fairness_foundation))
min_max = function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
df_numeric_norm = as.data.frame(lapply(df_numeric, min_max))
colnames(df_numeric_norm) = paste(colnames(df_numeric_norm),"norm",sep="_")
df_new = cbind(df, df_numeric_norm)

#rename variables
df_new = df_new %>% rename("moral_outrage" = "vice_sum_100", "negativeWOM_volume" = "vice_virality_norm",
                           "followers" = "followers_count_norm", "following" = "following_count_norm")

####Import new topics & create models####
cocacola = df_new %>% filter(company == "cocacola")
topics_cocacola = read_csv("topics_cocacola.csv")
topics_cocacola = topics_cocacola %>% select(-X1)
colnames(topics_cocacola) = c("id","topic_individual")
cocacola = cocacola %>% left_join(topics_cocacola, by = "id")
cocacola$topic_individual = as.factor(cocacola$topic_individual)

fit_cocacola_new = lm(moral_outrage ~ 
                        negativeWOM_volume +
                        relevel(max_morality, ref = "intensity_fairness")+
                        relevel(topic_individual, ref = "4") +
                        followers + following, 
                      data = cocacola)

vw = df_new2 %>% filter(company == "vw")
topics_vw = read_csv("topics_vw.csv")
topics_vw = topics_vw %>% select(-X1)
colnames(topics_vw) = c("id","topic_individual")
vw = vw %>% left_join(topics_vw, by = "id")
vw$topic_individual = as.factor(vw$topic_individual)

fit_vw_new = lm(moral_outrage ~ 
                  negativeWOM_volume +
                  relevel(max_morality, ref = "intensity_fairness")+ 
                  relevel(topic_individual, ref = "5") +
                  followers + following, 
                data = vw)

exxonmobil = df_new2 %>% filter(company == "exxonmobil")
topics_exxonmobil = read_csv("topics_exxonmobil.csv")
topics_exxonmobil = topics_exxonmobil %>% select(-X1)
colnames(topics_exxonmobil) = c("id","topic_individual")
exxonmobil = exxonmobil %>% left_join(topics_exxonmobil, by = "id")
exxonmobil$topic_individual = as.factor(exxonmobil$topic_individual)

fit_exxonmobil_new = lm(moral_outrage ~ 
                          negativeWOM_volume +
                          relevel(max_morality, ref = "intensity_fairness")+ 
                          relevel(topic_individual, ref = "5") +
                          followers + following, 
                        data = exxonmobil)

shell = df_new2 %>% filter(company == "shell")
topics_shell = read_csv("topics_shell.csv")
topics_shell = topics_shell %>% select(-X1)
colnames(topics_shell) = c("id","topic_individual")
shell = shell %>% left_join(topics_shell, by = "id")
shell$topic_individual = as.factor(shell$topic_individual)

fit_shell_new = lm(moral_outrage ~ 
                     negativeWOM_volume +
                     relevel(max_morality, ref = "intensity_fairness")+ 
                     relevel(topic_individual, ref = "2") +
                     followers + following, 
                   data = shell)

nestle = df_new2 %>% filter(company == "nestle")
topics_nestle = read_csv("topics_nestle.csv")
topics_nestle = topics_nestle %>% select(-X1)
colnames(topics_nestle) = c("id","topic_individual")
nestle = nestle %>% left_join(topics_nestle, by = "id")
nestle$topic_individual = as.factor(nestle$topic_individual)

fit_nestle_new = lm(moral_outrage ~ 
                      negativeWOM_volume +
                      relevel(max_morality, ref = "intensity_fairness")+ 
                      relevel(topic_individual, ref = "1") +
                      followers + following, 
                    data = nestle)

hm = df_new2 %>% filter(company == "hm")
fit_hm_new = lm(moral_outrage ~ 
                  negativeWOM_volume +
                  relevel(max_morality, ref = "intensity_fairness")+ 
                  relevel(topic_individual, ref = "2") +
                  followers + following, 
                data = hm)

unilever = df_new2 %>% filter(company == "unilever")
topics_unilever = read_csv("topics_unilever.csv")
topics_unilever = topics_unilever %>% select(-X1)
colnames(topics_unilever) = c("id","topic_individual")
unilever = unilever %>% left_join(topics_unilever, by = "id")
unilever$topic_individual = as.factor(unilever$topic_individual)

fit_unilever_new = lm(moral_outrage ~ 
                        negativeWOM_volume +
                        relevel(max_morality, ref = "intensity_fairness")+ 
                        relevel(topic_individual, ref = "4") +
                        followers + following, 
                      data = unilever)

ikea = df_new2 %>% filter(company == "ikea")
topics_ikea = read_csv("topics_ikea.csv")
topics_ikea = topics_ikea %>% select(-X1)
colnames(topics_ikea) = c("id","topic_individual")
ikea = ikea %>% left_join(topics_ikea, by = "id")
ikea$topic_individual = as.factor(ikea$topic_individual)

fit_ikea_new = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    relevel(max_morality, ref = "intensity_fairness")+ 
                    relevel(topic_individual, ref = "2") +
                    followers + following, 
                  data = ikea)

compare_company_new = export_summs(fit_cocacola_new,fit_exxonmobil_new,fit_hm_new,fit_ikea_new,fit_nestle_new,fit_shell_new,fit_unilever_new,fit_vw_new,
                                   model.names = c("Coca Cola", "Exxonmobil", "H&M","IKEA","Nestle","Shell","Unilever","Volkswagen"))
