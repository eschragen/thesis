####LIBRARIES####
library(tidyverse)
library(ggpubr)
library(rstatix)
library(multcomp)
library(car)

####IMPORT DATA####
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
df$env_claim_made = as.factor(df$env_claim_made)
df$max_morality = as.factor(df$max_morality)
df$topic = as.factor(df$topic)
df$industry_sensitive = as.factor(df$industry_sensitive)

#create ordered factors
df$revenues_class = factor(df$revenues_class, order = TRUE, levels = c("low", "mid","high"))

#exclude DV values > 99th quantile
df_new = df %>% filter(vice_sum_100 <= quantile(df$vice_sum_100, 100))

#1. ENVIRONMENTALLY SENSITIVE INDUSTRIES = MORE OUTRAGE####
#descriptive
df_new %>% group_by(industry_sensitive) %>% get_summary_stats(vice_sum_100)

#run t-test
t.test(vice_sum ~ industry_sensitive, data = df_new)

#run t-test with transformed data
t.test(log(vice_sum+1) ~ industry_sensitive, data = df_new)

#run nonparametric test (DV not normally distributed)
stat.test.wilcox = df_new %>% wilcox_test(vice_sum ~ industry_sensitive) %>% add_significance() %>% add_xy_position(x = "industry_sensitive")

#effect size: small effect size detected, r = 0.229
df_new %>% wilcox_effsize(vice_sum ~ industry_sensitive)

#visualize
bxp = ggboxplot(
  df_new, x = "industry_sensitive", y = "vice_sum_100", 
  ylab = "Moral Outrage", xlab = "Sensitive Industry")
bxp + stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test.wilcox, detailed = TRUE))

##report
# The average amount of moral outrage in environmental sensitive group was 1.75 (IQR = 2.78), 
# whereas the average amount of moral outrage in non-environmental sensitive group was 2.63 (IQR = 3.17). 
# The Wilcoxon test showed that the difference was significant (p < 0.0001, effect size r = 0.229).

#2. SIZE OF COMPANY = MORE OUTRAGE####
#REVENUE CLASSES
#descriptive
df_new %>% group_by(revenues_class) %>% get_summary_stats(vice_sum_100)

#visualize
ggboxplot(df_new, x = "revenues_class", y = "vice_sum_100", 
          color = "revenues_class", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("low", "mid", "high"),
          ylab = "Moral Outrage", xlab = "Company Size")

# Compute the analysis of variance
res.aov.size = aov(vice_sum_100 ~ revenues_class, data = df_new)
# Summary of the analysis
summary(res.aov.size)

#pairwise comparison
summary(glht(res.aov.size, linfct = mcp(revenues_class = "Tukey")))

#check assumptions
# 1. Homogeneity of variances (no pattern, p-value > 0.05)
plot(res.aov.size, 1)
leveneTest(vice_sum_100 ~ revenues_class, data = df_new)
# If violated: ANOVA test with no assumption of equal variances
oneway.test(vice_sum_100 ~ revenues_class, data = df_new)
#Or: Pairwise t-tests with no assumption of equal variances
pairwise.t.test(df_new$vice_sum_100, df_new$revenues_class,p.adjust.method = "BH", pool.sd = FALSE)

# 2. Normality
plot(res.aov.size, 2)
# If violated: Kruskal-Wallis rank sum test
kruskal.test(vice_sum_100 ~ revenues_class, data = df_new)

#REVENUE METRIC

#3. CONSUMER ALLEGATION####
#
