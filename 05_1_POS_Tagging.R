####OPTION 1: NLP ####

# options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
# gc()
# 
# library(NLP)
# library(tm)               # Load this prior to openNLP!
# library(openNLP)
# library(openNLPmodels.en)
# library(stringr)
# library(gsubfn)
# library(plyr)
# 
# source("~/GitHub/thesis/04_clean_tweets.R")
# #detach tidyverse & ggplot2 before running code
# detach("package:tidyverse", unload = TRUE)
# detach("package:ggplot2", unload = TRUE)
# 
# corpus.tmp = lapply(tweets_corpus_clean, function(x) {
#   x = paste(x, collapse = " ")
# })
# 
# corpus.tmp = unlist(corpus.tmp)
# 
# corpus.tmp = lapply(corpus.tmp, function (x) {
#   x = enc2utf8(x)
# })
# 
# corpus.tmp = gsub(" {2,}", " ", corpus.tmp)
# corpus.tmp = str_trim(corpus.tmp, side = "both")
# 
# Corpus = lapply(corpus.tmp, function(x) {
#   x = as.String(x)
# })
# 
# Corpus.tagged = lapply(Corpus, function(x) {
#   sent_token_annotator = Maxent_Sent_Token_Annotator()
#   word_token_annotator = Maxent_Word_Token_Annotator()
#   pos_tag_annotator = Maxent_POS_Tag_Annotator()
#   y1 = annotate(x, list(sent_token_annotator, word_token_annotator))
#   y2 = annotate(x, pos_tag_annotator, y1)
#   y2w = subset(y2, type == "word")
#   tags = sapply(y2w$features, '[[', "POS")
#   r1 = sprintf("%s/%s", x[y2w], tags)
#   r2 = paste(r1, collapse = " ")
#   return(r2) } )
  

####OPTION 2: koRpus TREE TAGGING####
options(scipen=999) #avoid scientific notations (e.g. e+18)
library(koRpus)
library(koRpus.lang.en)
library(tidyverse)
source("~/GitHub/thesis/04_clean_tweets.R")

#convert cleaned corpus to txt file
text = sapply(as.data.frame(paste(content$id, " ", content$tweet)), function(i)toString(i[!is.na(i)]))
text = gsub(",", "", text) #Remove comma
write.table(text, file = "textfile.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

#Run Tree Tagging
text.tagged = treetag("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\DATA\\textfile.txt",
                      treetagger = "manual", lang = "en",
                      TT.options = list(path = "C:\\TreeTagger", preset = "en"))

#Extract Speech Assignments
options(scipen = 100)
tags = text.tagged@tokens
tags_df = tags %>% select(token, wclass) 

#Replace wrong word assignments for numbers
tags_df$id = str_extract(tags_df$token, "[[:digit:]]+")  # Create new column for Digits

#convert factor of word class to character
tags_df$wclass = lapply(tags_df$wclass, as.character)

for(i in 1:nrow(tags_df)){
  if(!is.na(tags_df$id[i])){
    tags_df$wclass[i] = "user_id"
  }
}

#select word classes
tags_df = tags_df %>%  filter(wclass == "noun" | wclass == "adjective" | wclass == "verb" | wclass == "user_id")

#Loop: Prepare Spread (Goal: One line per ID) 
count = 1
user = 0
for (i in 1:length(tags_df$wclass)) {
  if(tags_df$wclass[i] != "user_id") {
    tags_df$number[i] = count
    count = count + 1
    tags_df$user[i] = user
  } else{
    count = 1
    tags_df$number[i]= count
    count = count + 1 
    user = user + 1
    tags_df$user[i] = user
  }
}

tags_df_spread = tags_df %>% select(-wclass, -id) %>% group_by(user) %>% spread(number, token) 
tags_df_spread = tags_df_spread[,-1]
content = tags_df_spread  %>% unite(tweet, sep =" ", remove = TRUE, na.rm = TRUE)

#extract id
content$id = str_extract(content$tweet, "[[:digit:]]+")  # Create new column
content$tweet = gsub("[[:digit:]]+", "", content$tweet) #Remove numbers

#Create corpus
tweets_POS_corpus = VCorpus(VectorSource(content$tweet))

