#Apply Scoring without POS tagging 
source("~/GitHub/thesis/04_clean_tweets.R")
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")

#Export lemmatized tweets
content_lem = as.data.frame(sapply(tweets_corpus_stemmed, function(x){x$content}))     #transponse (t) when you apply lemmatization
content_lem = cbind(tweets_subset[,1], content_lem)
colnames(content_lem) = c("id", "tweet")
content_lem[content_lem==""] = NA
content_lem = content_lem[complete.cases(content_lem),]
write.csv(content_lem, file = "content_lem.csv")

#Export non-lemmatized tweets
content[content==""] = NA
content = content[complete.cases(content),]
write.csv(content, file = "content_nonlem.csv")

# #Run eMFD Scoring via CMD
# cd C:\Users\eva_s\OneDrive\Dokumente\GitHub\Moral_Foundation_FrameAxis
# python "emfdcode.py"

### # Run eMFD via ANACONDA####
# #Create new df
# content_anaconda_lem = content_lem[,2]
# write.table(content_anaconda_lem, file = "content_anaconda_lem.csv", row.names = FALSE, col.names = FALSE)

# # Activate manually text wrapping!!!
# cd C:\Users\eva_s\OneDrive\Dokumente\GitHub\emfdscore

# conda create -n emfd python=3.7
# conda activate emfd
# conda install -c conda-forge spacy
# python -m spacy download en_core_web_sm
# pip install https://github.com/medianeuroscience/emfdscore/archive/master.zip
# pip install seaborn
# python emdscript.py

# # Import Scoring for Tweets
#anaconda = read_csv("results_emfd/results_emfdscore.csv")
#anaconda_emfd = cbind(content_lem, anaconda)
#### ####

cmd = read_csv("results_emfd/results.csv")
cmd = cmd[,-1]

cmd_emfd = cmd %>% select(id, tweet, bias_loyalty, bias_fairness, bias_sanctity, bias_authority, bias_care) %>% group_by(id) %>% 
  mutate(MFT_score = mean(bias_loyalty, bias_fairness, bias_sanctity, bias_authority, bias_care))

#Add MFT score to original df
df_subset = df_subset %>% left_join(cmd_emfd, by = "id") 


####VISUALIZATION####
#MFT per company
library(ggplot2)
MFTcompany = df_subset %>% select(id, company, MFT_score) %>% ggplot(aes(x = factor(company), y = MFT_score)) + 
  geom_bar(stat = "summary", fun = "mean")

MFTcompany

#MFT per topic (Run Topic Modeling before & this script w/o sourcing "Clean Tweets" (line 2)!)
MFTtopic = df_subset %>% select(id, MFT_score, topic) %>% ggplot(aes(x = as.factor(topic), y = MFT_score)) +
  geom_boxplot()

MFTtopic
