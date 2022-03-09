####LIBRARIES####
library(tidyverse)
library(jtools)
library(Rcpp)
library(mctest)
library(ggplot2)
library(stats)
library(patchwork)
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

####Import new topics & create models####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints/seperate_topic_moldeing_per_company")
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

vw = df_new %>% filter(company == "vw")
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

exxonmobil = df_new %>% filter(company == "exxonmobil")
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

shell = df_new %>% filter(company == "shell")
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

nestle = df_new %>% filter(company == "nestle")
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

hm = df_new %>% filter(company == "hm")
topics_hm = read_csv("topics_hm.csv")
topics_hm = topics_hm %>% select(-X1)
colnames(topics_hm) = c("id","topic_individual")
hm = hm %>% left_join(topics_hm, by = "id")
hm$topic_individual = as.factor(hm$topic_individual)
fit_hm_new = lm(moral_outrage ~ 
                  negativeWOM_volume +
                  relevel(max_morality, ref = "intensity_fairness")+ 
                  relevel(topic_individual, ref = "2") +
                  followers + following, 
                data = hm)

unilever = df_new %>% filter(company == "unilever")
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

ikea = df_new %>% filter(company == "ikea")
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
#compare_company_new


####ASSUMPTION TESTING####
#COOKS
# par(mfrow=c(2,4))
# plot(fit_cocacola_new, 4, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_exxonmobil_new, 4, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_hm_new, 4, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_ikea_new, 4, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_nestle_new, 4, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_shell_new, 4, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_unilever_new, 4, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)
# plot(fit_vw_new, 4, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, id.n = 0)

#extract top 10 highest cooks distance
top10cooks_ikea = ikea[c(which(rownames(ikea) %in% names(sort(cooks.distance(fit_ikea_new), decreasing = T)[1:10]))),]
ids_notoutlier_ikea = as.data.frame(ikea$id[!ikea$id %in% top10cooks_ikea$id])
colnames(ids_notoutlier_ikea) = "id"
ikea2 = ids_notoutlier_ikea %>% left_join(ikea, by = "id")

top10cooks_cocacola = cocacola[c(which(rownames(cocacola) %in% names(sort(cooks.distance(fit_cocacola_new), decreasing = T)[1:10]))),]
ids_notoutlier_cocacola = as.data.frame(cocacola$id[!cocacola$id %in% top10cooks_cocacola$id])
colnames(ids_notoutlier_cocacola) = "id"
cocacola2 = ids_notoutlier_cocacola %>% left_join(cocacola, by = "id")

top10cooks_exxonmobil = exxonmobil[c(which(rownames(exxonmobil) %in% names(sort(cooks.distance(fit_exxonmobil_new), decreasing = T)[1:10]))),]
ids_notoutlier_exxonmobil = as.data.frame(exxonmobil$id[!exxonmobil$id %in% top10cooks_exxonmobil$id])
colnames(ids_notoutlier_exxonmobil) = "id"
exxonmobil2 = ids_notoutlier_exxonmobil %>% left_join(exxonmobil, by = "id")

top10cooks_hm = hm[c(which(rownames(hm) %in% names(sort(cooks.distance(fit_hm_new), decreasing = T)[1:10]))),]
ids_notoutlier_hm = as.data.frame(hm$id[!hm$id %in% top10cooks_hm$id])
colnames(ids_notoutlier_hm) = "id"
hm2 = ids_notoutlier_hm %>% left_join(hm, by = "id")

top10cooks_nestle = nestle[c(which(rownames(nestle) %in% names(sort(cooks.distance(fit_nestle_new), decreasing = T)[1:10]))),]
ids_notoutlier_nestle = as.data.frame(nestle$id[!nestle$id %in% top10cooks_nestle$id])
colnames(ids_notoutlier_nestle) = "id"
nestle2 = ids_notoutlier_nestle %>% left_join(nestle, by = "id")

top10cooks_shell = shell[c(which(rownames(shell) %in% names(sort(cooks.distance(fit_shell_new), decreasing = T)[1:10]))),]
ids_notoutlier_shell = as.data.frame(shell$id[!shell$id %in% top10cooks_shell$id])
colnames(ids_notoutlier_shell) = "id"
shell2 = ids_notoutlier_shell %>% left_join(shell, by = "id")

top10cooks_unilever = unilever[c(which(rownames(unilever) %in% names(sort(cooks.distance(fit_unilever_new), decreasing = T)[1:10]))),]
ids_notoutlier_unilever = as.data.frame(unilever$id[!unilever$id %in% top10cooks_unilever$id])
colnames(ids_notoutlier_unilever) = "id"
unilever2 = ids_notoutlier_unilever %>% left_join(unilever, by = "id")

top10cooks_vw = vw[c(which(rownames(vw) %in% names(sort(cooks.distance(fit_vw_new), decreasing = T)[1:10]))),]
ids_notoutlier_vw = as.data.frame(vw$id[!vw$id %in% top10cooks_vw$id])
colnames(ids_notoutlier_vw) = "id"
vw2 = ids_notoutlier_vw %>% left_join(vw, by = "id")

#create new models
fit_cocacola_new2 = lm(moral_outrage ~ 
                        negativeWOM_volume +
                        relevel(max_morality, ref = "intensity_fairness")+
                        relevel(topic_individual, ref = "4") +
                        followers + following, 
                        data = cocacola2)

fit_vw_new2 = lm(moral_outrage ~ 
                  negativeWOM_volume +
                  relevel(max_morality, ref = "intensity_fairness")+ 
                  relevel(topic_individual, ref = "5") +
                  followers + following, 
                data = vw2)

fit_exxonmobil_new2 = lm(moral_outrage ~ 
                          negativeWOM_volume +
                          relevel(max_morality, ref = "intensity_fairness")+ 
                          relevel(topic_individual, ref = "5") +
                          followers + following, 
                        data = exxonmobil2)

fit_shell_new2 = lm(moral_outrage ~ 
                     negativeWOM_volume +
                     relevel(max_morality, ref = "intensity_fairness")+ 
                     relevel(topic_individual, ref = "2") +
                     followers + following, 
                   data = shell2)
fit_nestle_new2 = lm(moral_outrage ~ 
                      negativeWOM_volume +
                      relevel(max_morality, ref = "intensity_fairness")+ 
                      relevel(topic_individual, ref = "1") +
                      followers + following, 
                    data = nestle2)

fit_hm_new2 = lm(moral_outrage ~ 
                  negativeWOM_volume +
                  relevel(max_morality, ref = "intensity_fairness")+ 
                  relevel(topic_individual, ref = "2") +
                  followers + following, 
                data = hm2)


fit_unilever_new2 = lm(moral_outrage ~ 
                        negativeWOM_volume +
                        relevel(max_morality, ref = "intensity_fairness")+ 
                        relevel(topic_individual, ref = "4") +
                        followers + following, 
                      data = unilever2)


fit_ikea_new2 = lm(moral_outrage ~ 
                    negativeWOM_volume +
                    relevel(max_morality, ref = "intensity_fairness")+ 
                    relevel(topic_individual, ref = "2") +
                    followers + following, 
                  data = ikea2)

# #LINEARITY
# plot(fit_cocacola_new2, 1, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
# plot(fit_exxonmobil_new2, 1, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_hm_new2, 1, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_ikea_new2, 1, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_nestle_new2, 1, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_shell_new2, 1, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_unilever_new2, 1, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_vw_new2, 1, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# 
# #NORMALITY
# plot(fit_cocacola_new2, 2, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
# plot(fit_exxonmobil_new2, 2, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_hm_new2, 2, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_ikea_new2, 2, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_nestle_new2, 2, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_shell_new2, 2, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_unilever_new2, 2, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_vw_new2, 2, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# 
# #HOMO
# plot(fit_cocacola_new2, 3, sub.caption = NA, main = "Coca Cola", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0, lwd = 3)
# plot(fit_exxonmobil_new2, 3, sub.caption = NA,main = "ExxonMobil", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_hm_new2, 3, sub.caption = NA,main = "H&M", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_ikea_new2, 3, sub.caption = NA,main = "IKEA", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_nestle_new2, 3, sub.caption = NA,main = "Nestle", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_shell_new2, 3, sub.caption = NA,main = "Shell", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_unilever_new2, 3, sub.caption = NA,main = "Unilever", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# plot(fit_vw_new2, 3, sub.caption = NA,main = "VW", caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 3, id.n = 0,lwd = 3)
# 
# #MC
# imcdiag(fit_cocacola_new2)
# imcdiag(fit_exxonmobil_new2)
# imcdiag(fit_hm_new2)
# imcdiag(fit_ikea_new2)
# imcdiag(fit_nestle_new2)
# imcdiag(fit_shell_new2)
# imcdiag(fit_unilever_new2)
# imcdiag(fit_vw_new2)


####VISUALIZE MEAN DIFFERENCES OF MORAL OUTRAGE PER COMPANY####
means_topic_ikea = ikea %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_ikea = ikea %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics IKEA") + 
  stat_summary(fun="mean", size = .3)+ 
  # geom_text(size = 3.5, data = means_topic_ikea, aes(label = round(mean,2), y = mean + 0.2))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_unilever = unilever %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_unilever = unilever %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Unilever") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_unilever, aes(label = round(mean,2), y = mean - 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_cocacola = cocacola %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_cocacola = cocacola %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Coca-Cola") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_cocacola, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_exxonmobil = exxonmobil %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_exxonmobil = exxonmobil %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics ExxonMobil") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_exxonmobil, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_hm = hm %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_hm = hm %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics H&M") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_hm, aes(label = round(mean,2), y = mean + 0.6))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_vw = vw %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_vw = vw %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics VW") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_vw, aes(label = round(mean,2), y = mean + 0.4))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_nestle = nestle %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_nestle = nestle %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Nestle") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_nestle, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_shell = shell %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_shell = shell %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Shell") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_shell, aes(label = round(mean,2), y = mean + 0.4))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))


topic_plot_cocacola + topic_plot_exxonmobil + topic_plot_hm + topic_plot_ikea +
  topic_plot_nestle + topic_plot_shell + topic_plot_unilever + topic_plot_vw + plot_layout(nrow = 4)
