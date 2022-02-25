library(ggplot2)       
library(dplyr)          
library(tidyr)          
library(magrittr)       
library(gridExtra)      
library(readr)

df_select = read.csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")


####Comparison of Level 1 Industry Classification: T-Test####
#summary stats
summary(df_select %>% filter(industry1 == "Retail Trade") %>% .$bias_sum)
summary(df_select %>% filter(industry1 == "Manufacturing") %>% .$bias_sum)
ggplot(df_select, aes(industry1, bias_sum)) +   geom_boxplot()

#distributions: similar skewness given?
p1 = ggplot(sector_MFT, aes(MFT_score)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ industry1)

p2 = ggplot(sector_MFT, aes(MFT_score)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ industry1) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)

#run t-test
t.test(MFT_score ~ industry1, data = sector_MFT)

#run t-test with transformed data
t.test(log(MFT_score) ~ industry1, data = sector_MFT)

#run nonparametric test
wilcox.test(MFT_score ~ industry1, data = sector_MFT)




####Comparison of Petroleum vs. Other Sectors: T-Test####
#summary stats
sector_MFT$petroleum[sector_MFT$industry2 == "Petroleum Refining and Related Industries"] = "Petroleum Refining and Related Industries"
sector_MFT$petroleum[sector_MFT$industry2 != "Petroleum Refining and Related Industries"] = "Other Industries"

summary(sector_MFT %>% filter(petroleum == "Petroleum Refining and Related Industries") %>% .$MFT_score)
summary(sector_MFT %>% filter(petroleum == "Other Industries") %>% .$MFT_score)
ggplot(sector_MFT, aes(petroleum, MFT_score)) +   geom_boxplot()

#distributions: similar skewness given?
p1 = ggplot(sector_MFT, aes(MFT_score)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ petroleum)

p2 = ggplot(sector_MFT, aes(MFT_score)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ petroleum) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)

#run t-test
t.test(MFT_score ~ petroleum, data = sector_MFT)

#run t-test with transformed data
t.test(log(MFT_score) ~ petroleum, data = sector_MFT)

#run nonparametric test
wilcox.test(MFT_score ~ petroleum, data = sector_MFT)



####Comparison of Level 2 Industry Classification: ANOVA####
#Run Levene's test: Not significant = Variance is homogeneous = Assumption not violated :)
library(car)
sector_MFT$industry2 = as.factor(sector_MFT$industry2)
leveneTest(MFT_score~industry2,sector_MFT)

#Run ANOVA: Goal High F value, Low  P value
fit = aov(MFT_score~industry2,sector_MFT)
summary(fit)

#Describe means of individual groups
library(psych)
describeBy(sector_MFT$MFT_score, sector_MFT$industry2)

#Visualize differences
library(ggplot2)
ggplot(sector_MFT,aes(y=MFT_score, x=industry2, fill=industry2))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)
