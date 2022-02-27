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

####CREATE MODELS####
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

####ASSUMPTIONS####
#unusual observations
par(mfrow=c(1,5))
plot(fit_fairness, 4, sub.caption = NA, main = "Fairness", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_loyalty, 4, sub.caption = NA,main = "Loyalty", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_sanctity, 4, sub.caption = NA,main = "Sanctity", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_authority, 4, sub.caption = NA,main = "Authority", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
plot(fit_care, 4, sub.caption = NA,main = "Care", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)

#extract top 10 highest cooks distance
top10cooks_fairness = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_fairness), decreasing = T)[1:10]))),]
ids_notoutlier_fairness = as.data.frame(df_foundations$id[!df_foundations$id %in% top10cooks_fairness$id])
colnames(ids_notoutlier_fairness) = "id"
df_foundations2_fairness = ids_notoutlier_fairness %>% left_join(df_foundations, by = "id")

top10cooks_loyalty = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_loyalty), decreasing = T)[1:10]))),]
ids_notoutlier_loyalty = as.data.frame(df_foundations$id[!df_foundations$id %in% top10cooks_loyalty$id])
colnames(ids_notoutlier_loyalty) = "id"
df_foundations2_loyalty = ids_notoutlier_loyalty %>% left_join(df_foundations, by = "id")

top10cooks_sanctity = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_sanctity), decreasing = T)[1:10]))),]
ids_notoutlier_sanctity = as.data.frame(df_foundations$id[!df_foundations$id %in% top10cooks_sanctity$id])
colnames(ids_notoutlier_sanctity) = "id"
df_foundations2_sanctity = ids_notoutlier_sanctity %>% left_join(df_foundations, by = "id")

top10cooks_authority = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_authority), decreasing = T)[1:10]))),]
ids_notoutlier_authority = as.data.frame(df_foundations$id[!df_foundations$id %in% top10cooks_authority$id])
colnames(ids_notoutlier_authority) = "id"
df_foundations2_authority = ids_notoutlier_authority %>% left_join(df_foundations, by = "id")

top10cooks_care = df_foundations[c(which(rownames(df_foundations) %in% names(sort(cooks.distance(fit_care), decreasing = T)[1:10]))),]
ids_notoutlier_care = as.data.frame(df_foundations$id[!df_foundations$id %in% top10cooks_care$id])
colnames(ids_notoutlier_care) = "id"
df_foundations2_care = ids_notoutlier_care %>% left_join(df_foundations, by = "id")

#create new models
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
imcdiag(fit_loyalty2)
imcdiag(fit_authority2)
imcdiag(fit_sanctity2)
imcdiag(fit_care2)

#print summary statistics
compare_foundations = export_summs(fit_fairness2,fit_loyalty2,fit_authority2,fit_sanctity2,fit_care2,
                                   model.names = c("Fairness","Loyalty","Authority","Sanctity","Care"))
