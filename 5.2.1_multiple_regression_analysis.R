####LIBRARIES####
library(tidyverse)
library(jtools)
library(Rcpp)
library(mctest)
library(ggplot2)
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
stud_resids_df = as.data.frame(cbind(MASS::studres(fit), fitted(fit)))
colnames(stud_resids_df) = c("stud_res","fitted_values")

# #count observations with |studentized residuals| > 3
# outlier_n = stud_resids_df %>% filter(abs(stud_res) > 3) %>% nrow()     #1,317
# outlier_n/nrow(stud_resids_df)

ggplot(stud_resids_df, aes(x = fitted_values, y = stud_res)) + geom_point() +
  geom_hline(yintercept = 3, col = "red",lty = 2,lwd = 1) + xlab("Fitted Values") + ylab("Studentized Residuals") +
  geom_text(x = .8, y = 3.4,label = "Threshold",color="red",size = 4, check_overlap = T)+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=12)    , 
    text = element_text(size = 12),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.x =  element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.border = element_rect(colour = "black",fill=NA,size = .1)) 

names = names(residuals(fit))
cooks = as.data.frame(cooks.distance(fit))
colnames(cooks) = "value"
cooks$rank = rank(-cooks$value)
cooks = cooks %>% mutate(id = paste("#",rank, sep =""))

plot(fit, 4, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, cex.id = 1, sub.caption = NA, caption = NA, id.n= 10, labels.id = cooks$id)

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

#create summary statistics
summ(fit2, digits = 4)

####1. LINEARITY####
#Linearity between predictors and outcome variable (Residuals vs Fitted plot)
##Check: No fitted pattern (Red line approx. horizontal at Zero)
plot(fit2,1, sub.caption = NA ,  caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, lwd = 3)

####2. NORMALITYY#### 
#Normality of Residuals (Normal probability plot of residuals)
# #Check: Points fall approx. along reference line
# #Swings at left & right = Distribution of residuals heavier-tailed than theoretical distribution
plot(fit2, 2,sub.caption = NA , caption = NA, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, lwd = 3,id.n = 0)

# #Generate histogram of residual distribution (main="Distribution of Studentized Residuals")
sresid = MASS::studres(fit2)
hist(sresid, freq=FALSE,xlab = "Studentized Residuals", main = NA, cex.lab = 1.5,cex.axis = 1.5)
xfit=seq(min(sresid),max(sresid),length=40)
yfit=dnorm(xfit)
lines(xfit, yfit)

####3. HOMOSCEDASTICITY####
#Nonconstant Error Variance (Scale/Spread-location plot)
# #Horizontal line with equally spread points = residuals spread equally along ranges of predictors = Homogeneity of Variance
plot(fit2,3, sub.caption = NA , cex.lab = 1.5, cex.axis = 1.5, cex.main = 2.5, lwd = 3, caption = NA, id.n = 0)

####4. MULTICOLLINEARITY####
#Check VIF values (<5!)
# omcdiag(fit2)
# imcdiag(fit2)

#visualize interaction effect
df_interaction = df_new2 %>% select(green_ad, industry_brown, moral_outrage) %>%
  drop_na()

summary_interaction = df_interaction %>%
  group_by(green_ad, industry_brown) %>%
  summarise(outrage_mean = mean(moral_outrage),
            outrage_se = psych::describe(moral_outrage)$se)
summary_interaction %>% ggplot(aes(x = green_ad,y = outrage_mean,color = industry_brown)) +
  geom_line(aes(group = industry_brown), size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = outrage_mean - 1.96*outrage_se,
                    ymax = outrage_mean + 1.96*outrage_se),
                width = .1, size =1) +
  labs(x = "Green Advertising", color = "Brown Industry", y = "Moral Outrage") +
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 20),
    text = element_text(size = 24),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.x =  element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.border = element_rect(colour = "black",fill=NA,size = 1)) +
  scale_color_manual(values=c("#a9a9a9","#5C4033"))
