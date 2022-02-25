load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/cleaned.RData")

df = tweets_subset %>% sample_n(100) %>% select(tweet, id) 
df_text = as_vector(df[,1])

library(tidytext)
library(syuzhet)

nrc = get_nrc_sentiment(df_text, language = "english")

barplot(colSums(nrc),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')

nrc_tweets = cbind(df, nrc)

nrc_tweets_negative = nrc_tweets[,c(2,3,11)]

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd")
MFT = read_csv("results_withstopwords.csv")
MFT = MFT %>% group_by(id) %>% mutate(vice_sum = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice),
                                      bias_sum = sum(bias_loyalty, bias_care, bias_fairness, bias_sanctity, bias_authority)) 

MFT_comparison = MFT %>% select(id, vice_sum)

nrc_tweets_negative = nrc_tweets_negative %>% left_join(MFT_comparison, by = "id") %>% arrange(desc(vice_sum))
#%>% mutate(vice = 10*vice_sum) %>% select(-vice_sum)

nrc_tweets_negative$rownumber = 1:nrow(nrc_tweets_negative)

p1 = ggplot(nrc_tweets_negative, aes(x=rownumber,y=negative)) + geom_line() + geom_smooth() 
p2 = ggplot(nrc_tweets_negative, aes(x=rownumber,y=vice_sum)) + geom_line() + geom_smooth()
p3 = ggplot(nrc_tweets_negative, aes(x=rownumber,y=anger)) + geom_line() + geom_smooth()

library(patchwork)
p1+p2+p3
