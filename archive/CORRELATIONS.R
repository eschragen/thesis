library(DescTools)
library(tidyverse)
library(ltm)
library(psych)
library(ggplot2)

##CATEGORICAL - CATEGORICAL
#1. industry - revenue
table(df_new$industry_brown, df_new$revenues_class)
chisq.test(df_new$industry_brown, df_new$revenues_class) #p < 0.05 = Correlated
#STRONG POSITIVE ASSOCIATION!

#2. industry - env_claim
table(df_new$industry_brown, df_new$env_claim_made) 
chisq.test(df_new$industry_brown, df_new$env_claim_made) 

#3. industry - fairness
table(df_new$industry_brown, df_new$fairness_foundation) 
chisq.test(df_new$industry_brown, df_new$fairness_foundation) 

#4. industry - topic
table(df_new$industry_brown, df_new$topic) 
chisq.test(df_new$industry_brown, df_new$topic) 

#5. revenue - env_claim
table(df_new$revenues_class, df_new$env_claim_made) 
chisq.test(df_new$revenues_class, df_new$env_claim_made) 

#6. revenue - fairness
chisq.test(df_new$revenues_class, df_new$fairness_foundation)

#7. revenue - topic
#8. env_claim - fairness
#9. env_claim - topic
#10.fairness - topic

##NUMERICAL - NUMERICAL
#1. vice_virality_norm - followers_count_norm
#2. vice_virality_norm - following_count_norm
#3. followers_count_norm - following_count_norm
df_numeric = df_new %>% select(vice_virality_norm, followers_count_norm, following_count_norm) %>% drop_na()
cor(df_numeric)

##DICHOTOMOUS - NUMERICAL
#Point-Biserial Correlation (High = Bad)
#1. industry - vice_virality_norm
biserial.cor(y = df_new3$industry_brown, x = df_new3$vice_virality_norm, use = "complete.obs")
#2. industry - followers_count_norm
biserial.cor(y = df_new3$industry_brown, x = df_new3$followers_count_norm, use = "complete.obs")
#3. industry - following_count_norm
biserial.cor(y = df_new3$industry_brown, x = df_new3$following_count_norm, use = "complete.obs")
#4. env_claim - vice_virality_norm
biserial.cor(y = df_new3$industry_brown, x = df_new3$vice_virality_norm, use = "complete.obs")
#5. env_claim - followers_count_norm
biserial.cor(y = df_new3$env_claim_made, x = df_new3$followers_count_norm, use = "complete.obs")
#6. env_claim - following_count_norm
biserial.cor(y = df_new3$env_claim_made, x = df_new3$following_count_norm, use = "complete.obs")
#7. fairness - vice_virality_norm
biserial.cor(y = df_new3$fairness_foundation, x = df_new3$vice_virality_norm, use = "complete.obs")
#8. fairness - followers_count_norm
biserial.cor(y = df_new3$fairness_foundation, x = df_new3$followers_count_norm, use = "complete.obs")
#9. fairness - following_count_norm
biserial.cor(y = df_new3$fairness_foundation, x = df_new3$following_count_norm, use = "complete.obs")

#Wilcoxon test (High p = Good)
wilcox.test(df_new3$following_count_norm[which(df_new3$fairness_foundation == TRUE)], 
            df_new3$following_count_norm[which(df_new3$fairness_foundation == FALSE)])
wilcox.test(df_new3$following_count_norm[which(df_new3$industry_brown == 0)], 
            df_new3$following_count_norm[which(df_new3$industry_brown == 1)])
wilcox.test(df_new3$following_count_norm[which(df_new3$env_claim_made == 0)], 
            df_new3$following_count_norm[which(df_new3$env_claim_made == 1)])
wilcox.test(df_new3$followers_count_norm[which(df_new3$fairness_foundation == TRUE)], 
            df_new3$followers_count_norm[which(df_new3$fairness_foundation == FALSE)])
wilcox.test(df_new3$followers_count_norm[which(df_new3$industry_brown == 0)], 
            df_new3$followers_count_norm[which(df_new3$industry_brown == 1)])
wilcox.test(df_new3$followers_count_norm[which(df_new3$env_claim_made == 0)], 
            df_new3$followers_count_norm[which(df_new3$env_claim_made == 1)])
wilcox.test(df_new3$vice_virality_norm[which(df_new3$fairness_foundation == TRUE)], 
            df_new3$vice_virality_norm[which(df_new3$fairness_foundation == FALSE)])
wilcox.test(df_new3$vice_virality_norm[which(df_new3$industry_brown == 0)], 
            df_new3$vice_virality_norm[which(df_new3$industry_brown == 1)])
wilcox.test(df_new3$vice_virality_norm[which(df_new3$env_claim_made == 0)], 
            df_new3$vice_virality_norm[which(df_new3$env_claim_made == 1)])

##CATEGORICAL - NUMERICAL
#1. revenues - vice_virality_norm
ggplot(df_new3) + geom_boxplot(aes(revenues_class,vice_virality_norm))

#2. revenues - followers_count_norm
ggplot(df_new3) + geom_boxplot(aes(revenues_class,followers_count_norm))

#3. revenues - following_count_norm
ggplot(df_new3) + geom_boxplot(aes(revenues_class,following_count_norm))

#4. topic - vice_virality_norm 
ggplot(df_new3) + geom_boxplot(aes(topic,vice_virality_norm))

#5. topic - followers_count_norm
ggplot(df_new3) + geom_boxplot(aes(topic,followers_count_norm))

#6. topic - following_count_norm
ggplot(df_new3) + geom_boxplot(aes(topic,following_count_norm))
