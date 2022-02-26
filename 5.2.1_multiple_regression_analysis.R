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

####CREATE MODEL####
fit = lm(moral_outrage ~ 
              industry_brown  + industry_brown*green_ad +
              negativeWOM_volume +
              relevel(max_morality, ref = "intensity_fairness")+ 
              relevel(topic, ref = "4") +
              followers + following, 
              data = df_new)

####0. UNUSUAL OBSERVATIONS####
# #Plot studentized residuals vs. fitted values
# stud_resids_df = as.data.frame(cbind(MASS::studres(fit), fitted(fit)))
# colnames(stud_resids_df) = c("stud_res","fitted_values")
# 
# ggplot(stud_resids_df, aes(x = fitted_values, y = stud_res)) + geom_point() +
#   geom_hline(yintercept = 3, col = "red",lty = 2,lwd = 1) + xlab("Fitted Values") + ylab("Studentized Residuals") +
#   hrbrthemes::theme_ipsum(base_size =16,axis_title_size = 18)
#  
# plot(fit, 4, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, cex.id = 1, sub.caption = NA, caption = NA, id.n= 0)

#extract top 10 highest cooks distance
top10cooks = df_new[c(which(rownames(df_new) %in% names(sort(cooks.distance(fit), decreasing = T)[1:10]))),]
#combine in one df & exclude from original df (df_new)
top5outlier = rbind(top10cooks) # top5residuals,,top5influence,top5standresiduals)
ids_notoutlier = as.data.frame(df_new$id[!df_new$id %in% top5outlier$id])
colnames(ids_notoutlier) = "id"

df_new2 = ids_notoutlier %>% left_join(df_new, by = "id")

fit2 = lm(moral_outrage ~ 
            industry_brown  + industry_brown*green_ad +
            negativeWOM_volume +
            following + followers + 
            relevel(topic, ref = "4") +
            relevel(max_morality, ref = "intensity_fairness"),
            data = df_new2)

####1. LINEARITY####

#Linearity between predictors and outcome variable (Residuals vs Fitted plot)
##Check: No fitted pattern (Red line approx. horizontal at Zero)
#plot(fit2,1, sub.caption = NA , cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, lwd = 3)

####2. NORMALITYY#### 
#Normality of Residuals (Normal probability plot of residuals)
# #Check: Points fall approx. along reference line
# #Swings at left & right = Distribution of residuals heavier-tailed than theoretical distribution
svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Figures\\Assumptions\\4_normality_QQ.svg",bg = "transparent")
print(
  plot(fit2, 2)
)
dev.off()
# #Generate histogram of residual distribution
library(MASS)

# sresid = studres(fit2)
# svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Figures\\Assumptions\\4_normality_distribution.svg",bg = "transparent")
# print(
#   hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals", xlab = "Studentized Residuals"),
#   xfit=seq(min(sresid),max(sresid),length=40),
#   yfit=dnorm(xfit),
#   lines(xfit, yfit)
# )
# dev.off()

standres = stdres(fit)
svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Figures\\Assumptions\\4_normality_distribution_standardized.svg",bg = "transparent")
print(
  hist(standres, freq=FALSE,main="Distribution of Standardized Residuals", xlab = "Standardized Residuals"),
  xfit=seq(min(standres),max(standres),length=40),
  yfit=dnorm(xfit),
  lines(xfit, yfit)
)
dev.off()

####3. HOMOSCEDASTICITY####
#Nonconstant Error Variance (Scale/Spread-location plot)
# #Horizontal line with equally spread points = residuals spread equally along ranges of predictors = Homogeneity of Variance
plot(fit2,3, sub.caption = NA , cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, lwd = 3)


#ncvTest: p < 0.05 = Data is not homoscedastic :-(
library(car)
ncvTest(fit2)
#Check if roughly equally distributed across the range of the fitted Y values (straight line)
#spreadLevelPlot(fit2)

####4. MULTICOLLINEARITY####
#Check VIF values (<5!)
omcdiag(fit2)
imcdiag(fit2)




####COMPARE COMPANIES####
cocacola = df_new %>% filter(company == "cocacola")
fit_cocacola = lm(moral_outrage ~ 
            negativeWOM_volume +
            relevel(max_morality, ref = "intensity_fairness")+ 
            relevel(topic, ref = "4") +
            followers + following, 
            data = cocacola)


#add specific topics
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
fit_vw= lm(moral_outrage ~ 
                    
                    negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
                    followers + following, 
                  data = vw)

#add specific topics
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
fit_exxonmobil= lm(moral_outrage ~ 
             
             negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
             followers + following, 
           data = exxonmobil)

#add specific topics
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
fit_shell= lm(moral_outrage ~ 
             
             negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
             followers + following, 
           data = shell)

#add specific topics
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
fit_nestle= lm(moral_outrage ~ 
             
             negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
             followers + following, 
           data = nestle)

#add specific topics
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
fit_hm= lm(moral_outrage ~ 
                 
                 negativeWOM_volume +
             relevel(max_morality, ref = "intensity_fairness")+ 
             relevel(topic, ref = "4") +
                 followers + following, 
               data = hm)

#add specific topics
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

unilever = df_new2 %>% filter(company == "unilever")
fit_unilever= lm(moral_outrage ~ 
                 
                 negativeWOM_volume +
                   relevel(max_morality, ref = "intensity_fairness")+ 
                   relevel(topic, ref = "4") +
                 followers + following, 
               data = unilever)

#add specific topics
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
fit_ikea= lm(moral_outrage ~ 
                 
                 negativeWOM_volume +
               relevel(max_morality, ref = "intensity_fairness")+ 
               relevel(topic, ref = "4") +
                 followers + following, 
               data = ikea)

#add specific topics
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



compare_company = export_summs(fit_cocacola,fit_exxonmobil,fit_hm,fit_ikea,fit_nestle,fit_shell,fit_unilever,fit_vw,
             model.names = c("Coca Cola", "Exxonmobil", "H&M","IKEA","Nestle","Shell","Unilever","Volkswagen"))


compare_company_new = export_summs(fit_cocacola_new,fit_exxonmobil_new,fit_hm_new,fit_ikea_new,fit_nestle_new,fit_shell_new,fit_unilever_new,fit_vw_new,
                               model.names = c("Coca Cola", "Exxonmobil", "H&M","IKEA","Nestle","Shell","Unilever","Volkswagen"))



####COMPARE FOUNDATIONS####
df_foundations =  df_new2 %>% mutate(
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

compare_foundations = export_summs(fit_fairness, fit_loyalty,fit_sanctity,fit_authority,fit_care,
                                   model.names = c("Fairness","Loyalty","Sanctity","Authority","Care"), digits = 5)
compare_foundations
