library(readr)
library(tidyverse)
library(readxl)

#import all tweets
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df_nonequal_size")

retweet = df %>% select(id, replies_count, retweets_count, likes_count)

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd")
MFT = read_csv("results_withstopwords.csv")
MFT = MFT %>% group_by(id) %>% mutate(vice_sum = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice)) %>% select(id, vice_sum)

retweet_MFT = MFT %>% left_join(retweet, by = "id") 

retweet_MFT$contains_moral = NA

retweet_MFT$contains_moral[retweet_MFT$vice_sum > 0] = TRUE
retweet_MFT$contains_moral[retweet_MFT$vice_sum == 0] = FALSE

retweet_MFT$retweet_made = NA

retweet_MFT$retweet_made[retweet_MFT$retweets_count > 0] = TRUE
retweet_MFT$retweet_made[retweet_MFT$retweets_count == 0] = FALSE

library(ggplot2)

ggplot(retweet_MFT, aes(vice_sum, retweets_count)) + geom_point()

chisq.test(retweet_MFT$contains_moral, retweet_MFT$retweet_made)
mcnemar.test(retweet_MFT$contains_moral, retweet_MFT$retweet_made)
