####LIBRARIES####
library(tidyverse)
library(jtools)

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
df$year = as.factor(df$year)
df$topic1_TS = as.factor(df$topic1_TS)
df$topic2_TS = as.factor(df$topic2_TS)
df$topic3_TS = as.factor(df$topic3_TS)
df$topic4_TS = as.factor(df$topic4_TS)

#create ordered factors
df$revenues_class = factor(df$revenues_class, order = TRUE, levels = c("low", "mid","high"))

#exclude outliers
df = df %>% filter(following_count <= quantile(following_count, 0.95, na.rm = TRUE),
                   followers_count <= quantile(followers_count, 0.95, na.rm = TRUE),
                   vice_virality <= quantile(followers_count, 0.95, na.rm = TRUE))

#subset of metric features
df_numeric = df %>% select(-c(id, company, sic, industry1,industry2,industry_brown, revenues_class,
                              green_ad, max_morality, topic, year,
                              topic1_TS,topic2_TS,topic3_TS,topic4_TS, fairness_foundation))

# #normalize metric features
min_max = function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
df_numeric_norm = as.data.frame(lapply(df_numeric, min_max))
colnames(df_numeric_norm) = paste(colnames(df_numeric_norm),"norm",sep="_")

df_new = cbind(df, df_numeric_norm)

#rename variables
df_new = df_new %>% rename("moral_outrage" = "vice_sum_100", "negativeWOM_volume" = "vice_virality_norm",
                           "followers" = "followers_count_norm", "following" = "following_count_norm")

#create model
fit = lm(moral_outrage ~ 
           industry_brown  + industry_brown*green_ad +
           negativeWOM_volume +
           fairness_foundation + 
           relevel(topic, ref = "4") +
           followers + following, 
         data = df_new)

#extract top 10 highest cooks distance
top10cooks = df_new[c(which(rownames(df_new) %in% names(sort(cooks.distance(fit), decreasing = T)[1:10]))),]
#exclude from original df (df_new)
ids_notoutlier = as.data.frame(df_new$id[!df_new$id %in% top10cooks$id])
colnames(ids_notoutlier) = "id"
df_new2 = ids_notoutlier %>% left_join(df_new, by = "id")

####Run main model without outlier####
fit2 = lm(moral_outrage ~ 
            industry_brown  + industry_brown*green_ad +
            negativeWOM_volume +
            fairness_foundation + 
            relevel(topic, ref = "4") +
            followers + following, 
            data = df_new2)

####run model for every foundation####
model_fairness = lm(fairness.vice_100_log ~ 
            industry_brown  + industry_brown*green_ad +
            negativeWOM_volume + 
            relevel(topic, ref = "4") +
            followers + following, 
            data = df_new2)
model_loyalty = lm(loyalty.vice_100_log ~ 
                      industry_brown  + industry_brown*green_ad +
                      negativeWOM_volume + 
                      relevel(topic, ref = "4") +
                      followers + following, 
                    data = df_new2)
model_sanctity = lm(sanctity.vice_100_log ~ 
                     industry_brown  + industry_brown*green_ad +
                     negativeWOM_volume + 
                     relevel(topic, ref = "4") +
                     followers + following, 
                   data = df_new2)
model_authority = lm(authority.vice_100_log ~ 
                      industry_brown  + industry_brown*green_ad +
                      negativeWOM_volume + 
                      relevel(topic, ref = "4") +
                      followers + following, 
                    data = df_new2)
model_care = lm(care.vice_100_log ~ 
                      industry_brown  + industry_brown*green_ad +
                      negativeWOM_volume + 
                      relevel(topic, ref = "4") +
                      followers + following, 
                    data = df_new2)

compare_foundations = export_summs(model_authority,model_care,model_fairness, model_loyalty, model_sanctity,
                                   model.names = c("Authority", "Care","Fairness","Loyalty","Sanctity"))

####run model for every company####
df_cocacola = df_new2 %>% filter(company == "cocacola")
fit_cocacola = lm(moral_outrage ~ 
            negativeWOM_volume +
            fairness_foundation + 
            relevel(topic, ref = "4") +
            followers + following, 
            data = df_cocacola)

df_exxonmobil = df_new2 %>% filter(company == "exxonmobil")
fit_exxonmobil = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_exxonmobil)

df_hm = df_new2 %>% filter(company == "hm")
fit_hm = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_hm)

df_ikea = df_new2 %>% filter(company == "ikea")
fit_ikea = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_ikea)

df_nestle = df_new2 %>% filter(company == "nestle")
fit_nestle = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_nestle)

df_shell = df_new2 %>% filter(company == "shell")
fit_shell = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_shell)

df_unilever = df_new2 %>% filter(company == "unilever")
fit_unilever = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_unilever)

df_vw = df_new2 %>% filter(company == "vw")
fit_vw = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    fairness_foundation + 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_vw)

compare_companies = export_summs(fit_cocacola, fit_exxonmobil,fit_hm,fit_ikea,fit_nestle,fit_shell,fit_unilever,fit_vw,
                                 model.names = c("Coca Cola","Exxonmobil","H&M","IKEA","Nestle","Shell","Unilever","Volkswagen"))

compare_companies




####run supportive analysis per hypothesis####
#1.INDUSTRY (t-Test)
#2.INDUSTRY x GREEN AD 
#3.NEGATIVE WOM
#4.SOCIAL NETWORK SIZE
#5.TOPIC
#6.MORAL JUDGEMENT