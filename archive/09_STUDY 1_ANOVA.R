library(readr)
library(tidyverse)
df_select = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")

topic_MFT_df = df_select %>% select(company, topic, bias_sum) 
topic_MFT_df$topic = as.factor(topic_MFT_df$topic)

topic_MFT_df = topic_MFT_df %>% filter(company == "hm") %>%
  select(bias_sum, topic) %>% drop_na(topic)
invert100 = function(x) {return (100-(100*x))}
topic_MFT_df$bias_sum_new = invert100(topic_MFT_df$bias_sum)

#Run ANOVA: Goal High F value, Low  P value
fit = aov(bias_sum_new~topic,topic_MFT_df)
summary(fit)

TukeyHSD(fit)

library(multcomp)
summary(glht(fit, linfct = mcp(topic = "Tukey")))

pairwise.t.test(topic_MFT_df$bias_sum_new, topic_MFT_df$topic,
                p.adjust.method = "BH")

#Describe means of individual groups
library(psych)
describeBy(topic_MFT_df$bias_sum_new, topic_MFT_df$topic)

#Visualize differences
library(ggplot2)
ggplot(topic_MFT_df,aes(y=bias_sum_new, x=topic, fill=topic))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)
# library(ggpubr)
# ggboxplot(topic_MFT_df, x = "topic", y = "bias_sum_new", 
#           ylab = "Outrage", xlab = "Topic")
# ggline(topic_MFT_df, x = "topic", y = "bias_sum_new",
#        add = c("mean_se","jitter"),
#        ylab = "Outrage", xlab = "Topic")
# library(gplots)
# plotmeans(bias_sum_new~topic, data=topic_MFT_df, frame = FALSE,
#           xlab = "Topic", ylab = "Outrage")


#####CHECK ASSUMPTIONS####
#1. Homogeneity of variances --> Levene's test: Not significant = Variance is homogeneous = Assumption not violated :)
library(car)
leveneTest(bias_sum_new~topic,topic_MFT_df)

##Homogeneity of variance assumption violated? Alternatives:
#ANOVA test with no assumption of equal variances
oneway.test(bias_sum_new ~ topic, data = topic_MFT_df)
#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(topic_MFT_df$bias_sum_new, topic_MFT_df$topic,
                p.adjust.method = "BH", pool.sd = FALSE)

#2. Normality assumption: Fine if all points fall approx. along reference line
plot(fit, 2)
# Extract the residuals
aov_residuals = residuals(object = fit)
# Run Anderson-Darling Test 
library(nortest)
# # Run Shapiro-Wilk test if sample < 50000
# shapiro.test(x = aov_residuals)
ad.test(aov_residuals)

#Non-parametric alternative to one-way ANOVA test
kruskal.test(bias_sum_new ~ topic, data = topic_MFT_df)
