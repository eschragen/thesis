####LIBRARIES####
library(tidyverse)
library(jtools)
library(Rcpp)
library(mctest)
options(scipen=999) #avoid scientific notations

####PREPARE DATA####
data = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")
df = data %>% select(-c(X1,date,tweet,username,employees,total_assets)) 

#compute new output variable (LOG, Factor 100)
df = df %>% mutate(vice_sum_100 = 100*vice_sum,vice_sum_100_log = 100*log(1+vice_sum)) %>% drop_na(vice_sum)

#create factors
df$max_morality = as.factor(df$max_morality)
df$topic = as.factor(df$topic)


####TOPIC: RUN ANOVA####
#Run ANOVA: Goal High F value, Low  P value
fit = aov(vice_sum_100~topic,df)
summary(fit)

TukeyHSD(fit)

library(multcomp)
summary(glht(fit, linfct = mcp(topic = "Tukey")))

pairwise.t.test(df$vice_sum_100, df$topic,
                p.adjust.method = "BH")

#Describe means of individual groups
library(psych)
describeBy(df$vice_sum_100, df$topic)

#Visualize differences
library(ggplot2)
ggplot(df,aes(y=vice_sum_100, x=topic, fill=topic))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)

####TOPIC: TEST ASSUMPTIONS####
#1. Normality assumption: Fine if all points fall approx. along reference line
hist(df$vice_sum_100)
hist(df$vice_sum_100_log)
plot(fit, 2)
#Non-parametric alternative to one-way ANOVA test
kruskal.test(bias_sum_new ~ topic, data = topic_MFT_df)

#2. Homogeneity of variances --> Levene's test: Not significant = Variance is homogeneous = Assumption not violated :)
library(car)
leveneTest(vice_sum_100_log~topic,df)
leveneTest(vice_sum_100_log~max_morality,df)
##Homogeneity of variance assumption violated? Alternatives:
#ANOVA test with no assumption of equal variances
oneway.test(vice_sum_100 ~ topic, data = df)
#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(df$vice_sum_100, df$topic,
                p.adjust.method = "BH", pool.sd = FALSE)








####FOUNDATION: RUN ANOVA####
#Run ANOVA: Goal High F value, Low  P value
fit_moral = aov(vice_sum_100~max_morality,df)
summary(fit_moral)

TukeyHSD(fit_moral)

library(multcomp)
summary(glht(fit_moral, linfct = mcp(max_morality = "Tukey")))

pairwise.t.test(df$vice_sum_100, df$max_morality,
                p.adjust.method = "BH")

#Describe means of individual groups
library(psych)
describeBy(df$vice_sum_100, df$max_morality)

#Visualize differences
library(ggplot2)
ggplot(df,aes(y=vice_sum_100, x=max_morality, fill=max_morality))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)

####FOUNDATION: TEST ASSUMPTIONS####
#1. Normality assumption: Fine if all points fall approx. along reference line
hist(df$vice_sum_100)
hist(df$vice_sum_100_log)
plot(fit_moral, 2)
#Non-parametric alternative to one-way ANOVA test
kruskal.test(vice_sum_100 ~ max_morality, data = df)

#2. Homogeneity of variances --> Levene's test: Not significant = Variance is homogeneous = Assumption not violated :)
library(car)
leveneTest(vice_sum_100_log~max_morality,df)
leveneTest(vice_sum_100_log~max_morality,df)
##Homogeneity of variance assumption violated? Alternatives:
#ANOVA test with no assumption of equal variances
oneway.test(vice_sum_100 ~ max_morality, data = df)
#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(df$vice_sum_100, df$max_morality,
                p.adjust.method = "BH", pool.sd = FALSE)






