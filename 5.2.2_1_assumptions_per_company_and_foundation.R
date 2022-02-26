####LIBRARIES####
library(tidyverse)
library(jtools)
library(Rcpp)
library(mctest)
library(ggplot2)
library(stats)
library(MASS)
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
df$year = as.factor(df$year)

#create ordered factors
df$revenues_class = factor(df$revenues_class, order = TRUE, levels = c("low", "mid","high"))

#exclude outliers
df = df %>% filter(following_count <= quantile(following_count, 0.95, na.rm = TRUE),
                   followers_count <= quantile(followers_count, 0.95, na.rm = TRUE),
                   vice_virality <= quantile(followers_count, 0.95, na.rm = TRUE))

#subset of metric features
df_numeric = df %>% select(-c(id, company, sic, industry1,industry2,industry_brown, revenues_class,
                              green_ad, max_morality, topic, year,
                              fairness_foundation))

# #normalize metric features
min_max = function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
df_numeric_norm = as.data.frame(lapply(df_numeric, min_max))
colnames(df_numeric_norm) = paste(colnames(df_numeric_norm),"norm",sep="_")

df_new = cbind(df, df_numeric_norm)

#rename variables
df_new = df_new %>% rename("moral_outrage" = "vice_sum_100", "negativeWOM_volume" = "vice_virality_norm",
                           "followers" = "followers_count_norm", "following" = "following_count_norm")

####CREATE MODEL###
fit = lm(moral_outrage ~ 
           industry_brown  + industry_brown*green_ad +
           negativeWOM_volume +
           relevel(max_morality, ref = "intensity_fairness")+ 
           relevel(topic, ref = "4") +
           followers + following, 
          data = df_new)

#extract top 10 highest cooks distance
top10cooks = df_new[c(which(rownames(df_new) %in% names(sort(cooks.distance(fit), decreasing = T)[1:10]))),]
#combine in one df & exclude from original df (df_new)
top5outlier = rbind(top10cooks) # top5residuals,,top5influence,top5standresiduals)
ids_notoutlier = as.data.frame(df_new$id[!df_new$id %in% top5outlier$id])
colnames(ids_notoutlier) = "id"

df_new2 = ids_notoutlier %>% left_join(df_new, by = "id")

####CREATE MODELS####
####FOUNDATIONS####
df_foundations =  df_new %>% mutate(
  fairness.vice_100 = 100*fairness.vice,
  loyalty.vice_100= 100*loyalty.vice,
  sanctity.vice_100= 100*sanctity.vice,
  authority.vice_100 = 100*authority.vice,
  care.vice_100 = 100*care.vice) 

fit_fairness = lm(fairness.vice_100 ~ 
                    industry_brown  + industry_brown*green_ad +
                    negativeWOM_volume +
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_foundations)


fit_loyalty = lm(loyalty.vice_100 ~ 
                   industry_brown  + industry_brown*green_ad +
                   negativeWOM_volume +
                   relevel(topic, ref = "4") +
                   followers + following, 
                 data = df_foundations)


fit_sanctity = lm(sanctity.vice_100 ~ 
                    industry_brown  + industry_brown*green_ad +
                    negativeWOM_volume +
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_foundations)


fit_authority = lm(authority.vice_100 ~ 
                     industry_brown  + industry_brown*green_ad +
                     negativeWOM_volume +
                     relevel(topic, ref = "4") +
                     followers + following, 
                   data = df_foundations)

fit_care = lm(care.vice_100 ~ 
                industry_brown  + industry_brown*green_ad +
                negativeWOM_volume +
                relevel(topic, ref = "4") +
                followers + following, 
              data = df_foundations)

par(mfrow=c(1,5))
plot(fit_fairness, 4, sub.caption = NA, main = "Fairness", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_loyalty, 4, sub.caption = NA,main = "Loyalty", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_sanctity, 4, sub.caption = NA,main = "Sanctity", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_authority, 4, sub.caption = NA,main = "Authority", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_care, 4, sub.caption = NA,main = "Care", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)


#extract top 10 highest cooks distance
top10cooks_fairness = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_fairness), decreasing = T)[1:10]))),]
top5outlier_fairness = rbind(top10cooks_fairness) 
ids_notoutlier_fairness = as.data.frame(df_foundations$id[!df_foundations$id %in% top5outlier_fairness$id])
colnames(ids_notoutlier_fairness) = "id"
df_foundations2_fairness = ids_notoutlier_fairness %>% left_join(df_foundations, by = "id")

top10cooks_loyalty = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_loyalty), decreasing = T)[1:10]))),]
top5outlier_loyalty = rbind(top10cooks_loyalty) 
ids_notoutlier_loyalty = as.data.frame(df_foundations$id[!df_foundations$id %in% top5outlier_loyalty$id])
colnames(ids_notoutlier_loyalty) = "id"
df_foundations2_loyalty = ids_notoutlier_loyalty %>% left_join(df_foundations, by = "id")

top10cooks_sanctity = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_sanctity), decreasing = T)[1:10]))),]
top5outlier_sanctity = rbind(top10cooks_sanctity) 
ids_notoutlier_sanctity = as.data.frame(df_foundations$id[!df_foundations$id %in% top5outlier_sanctity$id])
colnames(ids_notoutlier_sanctity) = "id"
df_foundations2_sanctity = ids_notoutlier_sanctity %>% left_join(df_foundations, by = "id")

top10cooks_authority = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_authority), decreasing = T)[1:10]))),]
top5outlier_authority = rbind(top10cooks_authority) 
ids_notoutlier_authority = as.data.frame(df_foundations$id[!df_foundations$id %in% top5outlier_authority$id])
colnames(ids_notoutlier_authority) = "id"
df_foundations2_authority = ids_notoutlier_authority %>% left_join(df_foundations, by = "id")

top10cooks_care = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_care), decreasing = T)[1:10]))),]
top5outlier_care = rbind(top10cooks_care) 
ids_notoutlier_care = as.data.frame(df_foundations$id[!df_foundations$id %in% top5outlier_care$id])
colnames(ids_notoutlier_care) = "id"
df_foundations2_care = ids_notoutlier_care %>% left_join(df_foundations, by = "id")

#create new plots
fit_fairness2 = lm(fairness.vice_100 ~ 
                    industry_brown  + industry_brown*green_ad +
                    negativeWOM_volume +
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_foundations2_fairness)


fit_loyalty2 = lm(loyalty.vice_100 ~ 
                   industry_brown  + industry_brown*green_ad +
                   negativeWOM_volume +
                   relevel(topic, ref = "4") +
                   followers + following, 
                 data = df_foundations2_loyalty)


fit_sanctity2 = lm(sanctity.vice_100 ~ 
                    industry_brown  + industry_brown*green_ad +
                    negativeWOM_volume +
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = df_foundations2_sanctity)


fit_authority2 = lm(authority.vice_100 ~ 
                     industry_brown  + industry_brown*green_ad +
                     negativeWOM_volume +
                     relevel(topic, ref = "4") +
                     followers + following, 
                   data = df_foundations2_authority)

fit_care2 = lm(care.vice_100 ~ 
                industry_brown  + industry_brown*green_ad +
                negativeWOM_volume +
                relevel(topic, ref = "4") +
                followers + following, 
              data = df_foundations2_care)

####1. LINEARITY####
par(mfrow=c(1,5))
plot(fit_fairness2,1, sub.caption = NA,main = "Fairness", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_loyalty2,1, sub.caption = NA,main = "Loyalty", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_sanctity2,1, sub.caption = NA,main = "Sanctity", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_authority2,1, sub.caption = NA,main = "Authority", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_care2,1, sub.caption = NA,main = "Care", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)


####2. NORMALITYY#### 
par(mfrow=c(1,5))
plot(fit_fairness2,2, sub.caption = NA,main = "Fairness", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_loyalty2,2, sub.caption = NA,main = "Loyalty", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_sanctity2,2, sub.caption = NA,main = "Sanctity", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_authority2,2, sub.caption = NA,main = "Authority", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_care2,2, sub.caption = NA,main = "Care", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)


####3. HOMOSCEDASTICITY####
par(mfrow=c(1,5))
plot(fit_fairness2,3, sub.caption = NA,main = "Fairness", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_loyalty2,3, sub.caption = NA,main = "Loyalty", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_sanctity2,3, sub.caption = NA,main = "Sanctity", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_authority2,3, sub.caption = NA,main = "Authority", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)
plot(fit_care2,3, sub.caption = NA,main = "Care", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0, lwd = 5)

####4. MULTICOLLINEARITY####
imcdiag(fit_fairness2)


####COMPANIES####
cocacola = df_new %>% filter(company == "cocacola")
fit_cocacola = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    relevel(max_morality, ref = "intensity_fairness")+ 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = cocacola)

vw = df_new %>% filter(company == "vw")
fit_vw= lm(moral_outrage ~ 
             
             negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
             followers + following, 
           data = vw)

exxonmobil = df_new %>% filter(company == "exxonmobil")
fit_exxonmobil= lm(moral_outrage ~ 
                     
                     negativeWOM_volume +
                     relevel(max_morality, ref = "intensity_fairness")+ 
                     relevel(topic, ref = "4") +
                     followers + following, 
                   data = exxonmobil)

shell = df_new %>% filter(company == "shell")
fit_shell= lm(moral_outrage ~ 
                
                negativeWOM_volume +
                relevel(max_morality, ref = "intensity_fairness")+ 
                relevel(topic, ref = "4") +
                followers + following, 
              data = shell)

nestle = df_new %>% filter(company == "nestle")
fit_nestle= lm(moral_outrage ~ 
                 
                 negativeWOM_volume +
                 relevel(max_morality, ref = "intensity_fairness")+ 
                 relevel(topic, ref = "4") +
                 followers + following, 
               data = nestle)

hm = df_new %>% filter(company == "hm")
fit_hm= lm(moral_outrage ~ 
             
             negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
             followers + following, 
           data = hm)

unilever = df_new %>% filter(company == "unilever")
fit_unilever= lm(moral_outrage ~ 
                   
                   negativeWOM_volume +
                   relevel(max_morality, ref = "intensity_fairness")+ 
                   relevel(topic, ref = "4") +
                   followers + following, 
                 data = unilever)

ikea = df_new %>% filter(company == "ikea")
fit_ikea= lm(moral_outrage ~ 
               
               negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
               followers + following, 
              data=ikea)

#COOKS
par(mfrow=c(2,4))
plot(fit_cocacola, 4, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_exxonmobil, 4, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_hm, 4, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_ikea, 4, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_nestle, 4, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_shell, 4, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_unilever, 4, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_vw, 4, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)

#LINEARITY
plot(fit_cocacola, 1, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil, 1, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm, 1, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea, 1, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle, 1, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell, 1, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever, 1, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw, 1, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#NORMALITY
plot(fit_cocacola, 2, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil, 2, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm, 2, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea, 2, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle, 2, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell, 2, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever, 2, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw, 2, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#HOMO
plot(fit_cocacola, 3, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil, 3, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm, 3, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea, 3, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle, 3, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell, 3, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever, 3, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw, 3, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#MC
imcdiag(fit_cocacola)
imcdiag(fit_exxonmobil)
imcdiag(fit_hm)
imcdiag(fit_ikea)
imcdiag(fit_nestle)
imcdiag(fit_shell)
imcdiag(fit_unilever)
imcdiag(fit_vw)
