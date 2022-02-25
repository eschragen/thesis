library(tidyverse)
library(ggplot2)
library(GGally)
options(digits = 5) # reduce decimal places
options(scipen=999) #avoid scientific notations (e.g. e+18)
df_select = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")
df_select$company = as.factor(df_select$company)
df_select$sic = as.factor(df_select$sic)
df_select$industry1 = as.factor(df_select$industry1)
df_select$industry2 = as.factor(df_select$industry2)
df_select$env_claim_made = as.factor(df_select$env_claim_made)
df_select$max_morality = as.factor(df_select$max_morality)
df_select$topic = as.factor(df_select$topic)
df_select$topic_total_subset = as.factor(df_select$topic_total_subset)

#drop NAs of DV
df_select = df_select %>% drop_na(vice_sum)

#normalize metric IVs
normalize = function(x) {return ((x - min(x)) / (max(x) - min(x)))}

df_select$virality_norm = normalize(df_select$virality)
df_select$employees_norm = normalize(df_select$employees)
df_select$revenues_norm = normalize(df_select$revenues)
df_select$total_assets_norm = normalize(df_select$total_assets)
df_select$env_performance_norm = normalize(df_select$env_performance)
df_select$total_env_posts_perweek_norm = normalize(df_select$total_env_posts_perweek)

#select variables for analysis
df_analysis = df_select %>% mutate(log_vice = log(1+vice_sum)) 
df_analysis = df_analysis %>% select(-vice_sum) 
df_analysis = df_analysis %>% select(log_vice, revenues ,env_performance, env_claim_made, vice_virality, sic, topic)
df_analysis = df_analysis %>% drop_na()

summary(df_analysis)

#build model
model = lm(log_vice ~., data = df_analysis)
summary(model)

# ####Detect Outliers####
# #standardized residual > 3 = possible outliers
# 
# #high leverage points (hat-value)
# 
# ####1 Linearity of data####
# #goal: no pattern, red line approx. horizontal at zero
# #remedy: log, sqrt, x^2
plot(model8,1)
ggpairs(df_analysis, columns = c(1,5,6))
ggpairs(df8)
# 
# ####2 Normality of residuals####
# #goal: residuals follow straight line
# plot(model,2)
# 
# ####3 Homogeneity of residuals variance####
# residual errors have constant variance
# #goal: horizontal line with equally spread points
# #remedy: log, square root transformation of DV
plot(model8, 3)
#H0: constant variance --> aim: p > 0.05
ncvTest(model8)

####4 Independence of residuals error terms####

####5 Multicollinearity####
#check correlation
#metric variables
library(corrplot)
library(RColorBrewer)
M  = cor(df_analysis)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
#categorical: spearman rank (ordinal)/chi-square (nominal)
#categorical & metric: t-Test/ANOVA
#Predictors are independent and observed with neglogible error
#aim: not significant (H0: Errors are not auto-correlated with themselves = independent)
# memory.limit(9999999)
# durbinWatsonTest(model)

####GVLMA####
library(gvlma)
gvlma(model)

com = "unilever"
virality = df_select %>% filter(company==com) %>% select(date, vice_virality) %>% group_by(date) %>% summarise(virality_mean = mean(vice_virality)) 
bias = df_select %>% filter(company==com) %>% select(date, bias_sum) %>% group_by(date) %>% summarise(bias_mean = mean(bias_sum)) 
environmental_postings = df_select %>% filter(company==com) %>% select(date, total_env_posts_perweek) %>% group_by(date) %>% summarise(total_env_posts_perweek_mean = mean(total_env_posts_perweek)) 

virality_bias = cbind(virality, bias[,2], environmental_postings[,2])
 
p1= ggplot(virality_bias, aes(x=date, y = bias_mean)) +  geom_line() + geom_smooth()
p2= ggplot(virality_bias, aes(x=date, y = virality_mean)) +  geom_line() + geom_smooth()
p3= ggplot(virality_bias, aes(x=date, y = total_env_posts_perweek_mean)) +  geom_line() + geom_smooth()

library(patchwork)
p1+p2#+p3

cor(df_select$vice_virality,df_select$bias_sum)
model = lm(df_select$bias_sum~df_select$vice_virality,data=df_select)
summary(model)
