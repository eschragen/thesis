####LIBRARIES####
library(tidyverse)
library(ggplot2)
library(jtools)
library(Rcpp)
library(mctest)
library(stats)
options(scipen=999)

####PREPARE DATA####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
data = read_csv("df_select.csv")
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

####ASSUMPTIONS####
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

#extract top 10 highest cooks distance
top10cooks_ikea = ikea[c(which(rownames(ikea) %in% names(sort(cooks.distance(fit_ikea), decreasing = T)[1:10]))),]
ids_notoutlier_ikea = as.data.frame(ikea$id[!ikea$id %in% top10cooks_ikea$id])
colnames(ids_notoutlier_ikea) = "id"
ikea2 = ids_notoutlier_ikea %>% left_join(ikea, by = "id")

top10cooks_cocacola = cocacola[c(which(rownames(cocacola) %in% names(sort(cooks.distance(fit_cocacola), decreasing = T)[1:10]))),]
ids_notoutlier_cocacola = as.data.frame(cocacola$id[!cocacola$id %in% top10cooks_cocacola$id])
colnames(ids_notoutlier_cocacola) = "id"
cocacola2 = ids_notoutlier_cocacola %>% left_join(cocacola, by = "id")

top10cooks_exxonmobil = exxonmobil[c(which(rownames(exxonmobil) %in% names(sort(cooks.distance(fit_exxonmobil), decreasing = T)[1:10]))),]
ids_notoutlier_exxonmobil = as.data.frame(exxonmobil$id[!exxonmobil$id %in% top10cooks_exxonmobil$id])
colnames(ids_notoutlier_exxonmobil) = "id"
exxonmobil2 = ids_notoutlier_exxonmobil %>% left_join(exxonmobil, by = "id")

top10cooks_hm = hm[c(which(rownames(hm) %in% names(sort(cooks.distance(fit_hm), decreasing = T)[1:10]))),]
ids_notoutlier_hm = as.data.frame(hm$id[!hm$id %in% top10cooks_hm$id])
colnames(ids_notoutlier_hm) = "id"
hm2 = ids_notoutlier_hm %>% left_join(hm, by = "id")

top10cooks_nestle = nestle[c(which(rownames(nestle) %in% names(sort(cooks.distance(fit_nestle), decreasing = T)[1:10]))),]
ids_notoutlier_nestle = as.data.frame(nestle$id[!nestle$id %in% top10cooks_nestle$id])
colnames(ids_notoutlier_nestle) = "id"
nestle2 = ids_notoutlier_nestle %>% left_join(nestle, by = "id")

top10cooks_shell = shell[c(which(rownames(shell) %in% names(sort(cooks.distance(fit_shell), decreasing = T)[1:10]))),]
ids_notoutlier_shell = as.data.frame(shell$id[!shell$id %in% top10cooks_shell$id])
colnames(ids_notoutlier_shell) = "id"
shell2 = ids_notoutlier_shell %>% left_join(shell, by = "id")

top10cooks_unilever = unilever[c(which(rownames(unilever) %in% names(sort(cooks.distance(fit_unilever), decreasing = T)[1:10]))),]
ids_notoutlier_unilever = as.data.frame(unilever$id[!unilever$id %in% top10cooks_unilever$id])
colnames(ids_notoutlier_unilever) = "id"
unilever2 = ids_notoutlier_unilever %>% left_join(unilever, by = "id")

top10cooks_vw = vw[c(which(rownames(vw) %in% names(sort(cooks.distance(fit_vw), decreasing = T)[1:10]))),]
ids_notoutlier_vw = as.data.frame(vw$id[!vw$id %in% top10cooks_vw$id])
colnames(ids_notoutlier_vw) = "id"
vw2 = ids_notoutlier_vw %>% left_join(vw, by = "id")

#create new models
fit_cocacola2 = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    relevel(max_morality, ref = "intensity_fairness")+ 
                    relevel(topic, ref = "4") +
                    followers + following, 
                  data = cocacola2)

fit_vw2= lm(moral_outrage ~ 
             negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
             followers + following, 
           data = vw2)

fit_exxonmobil2= lm(moral_outrage ~ 
                     negativeWOM_volume +
                     relevel(max_morality, ref = "intensity_fairness")+ 
                     relevel(topic, ref = "4") +
                     followers + following, 
                   data = exxonmobil2)

fit_shell2= lm(moral_outrage ~ 
                negativeWOM_volume +
                relevel(max_morality, ref = "intensity_fairness")+ 
                relevel(topic, ref = "4") +
                followers + following, 
              data = shell2)

fit_nestle2= lm(moral_outrage ~ 
                 negativeWOM_volume +
                 relevel(max_morality, ref = "intensity_fairness")+ 
                 relevel(topic, ref = "4") +
                 followers + following, 
               data = nestle2)

fit_hm2= lm(moral_outrage ~ 
             negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
             followers + following, 
           data = hm2)

fit_unilever2= lm(moral_outrage ~ 
                   negativeWOM_volume +
                   relevel(max_morality, ref = "intensity_fairness")+ 
                   relevel(topic, ref = "4") +
                   followers + following, 
                 data = unilever2)

fit_ikea2= lm(moral_outrage ~ 
               negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
               followers + following, 
              data=ikea2)

#LINEARITY
plot(fit_cocacola2, 1, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil2, 1, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm2, 1, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea2, 1, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle2, 1, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell2, 1, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever2, 1, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw2, 1, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#NORMALITY
plot(fit_cocacola2, 2, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil2, 2, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm2, 2, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea2, 2, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle2, 2, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell2, 2, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever2, 2, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw2, 2, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#HOMO
plot(fit_cocacola2, 3, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
plot(fit_exxonmobil2, 3, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_hm2, 3, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_ikea2, 3, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_nestle2, 3, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_shell2, 3, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_unilever2, 3, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
plot(fit_vw2, 3, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)

#MC
imcdiag(fit_cocacola2)
imcdiag(fit_exxonmobil2)
imcdiag(fit_hm2)
imcdiag(fit_ikea2)
imcdiag(fit_nestle2)
imcdiag(fit_shell2)
imcdiag(fit_unilever2)
imcdiag(fit_vw2)

#print summary statistics
compare_company = export_summs(fit_cocacola2,fit_exxonmobil2,fit_hm2,fit_ikea2,fit_nestle2,fit_shell2,fit_unilever2,fit_vw2,
                               model.names = c("Coca Cola", "Exxonmobil", "H&M","IKEA","Nestle","Shell","Unilever","Volkswagen"))