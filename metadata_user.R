library(readr)
library(tidyverse)
library(readxl)

#import all tweets
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df_nonequal_size")

df_user = df %>% select(username) 
df_user = df_user[!duplicated(df_user$username),]
df_user = df_user$username


####TWITTER API####
library(academictwitteR)

bearer_token = "AAAAAAAAAAAAAAAAAAAAANlzVAEAAAAA8Pc5SBGAXjyNCgCdVWakBNQ2KEw%3DVT1f7rmvdKo43fqWH8xLfEU0TDBdDk3yGt1uk5yU9GRJEnsbSQ"
# 
# #1:17181
# 
# user_id = NA
# 
# for (i in 1:length(df_user)) {
#   user = get_user_id(df_user[i], bearer_token)
#   user_id[i] = user}
# 
# user_id = as.data.frame(user_id)
# user_id = user_id %>% filter(!is.na(user_id)) 
# user_id = user_id$user_id
# 
# user_profile = get_user_profile(user_id, bearer_token)
# 
# user_info = NA
# username = as.data.frame(user_profile$username)
# created = as.data.frame(user_profile$created_at)
# id = as.data.frame(user_profile$id)
# metrics = as.data.frame(user_profile$public_metrics)
# 
# user_profile = cbind(username, created, id, metrics)
# colnames(user_profile) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile, "user_profile.csv")

# #17182:19225


#remotes::install_github("mkearney/tweetbotornot2")
library(tweetbotornot2)
library(beepr)
library(tidyverse)
api_key = "NoHTJbhyaHP11wu7QzUVjq0ID"
api_secret_key = "opGq4aB6zCKQ3IYWz7hG2LTE8plRU2L8gdaw30PGOJNAsq1JcY"
access_token = "1446099424934043650-QUHRFd1zNvIWZoK2RpWWXS67Z7CcyT"
access_token_secret = "yq829koXHlMzWmmmvf8UlAbAu4Bc5rfg80tRjcNtWCb4D"

#install.packages("rtweet")
library(rtweet)
token = create_token(
  app = "predict bots",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#import all tweets
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
df = read_csv("df_nonequal_size")

df_user = df %>% select(username) 
df_user = df_user[!duplicated(df_user$username),]
df_user = df_user$username


# user = as.data.frame(df_user) 
# setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/userid")

# #19225:20000
# user_subset = user[19225:20000,]
# screen_name = data.frame(screen_name = user_subset)
# userid = predict_bot(screen_name, token = token)
# 
# write_csv(userid, "user1.csv")
# 
# #20000:25000
# user_subset = user[20000:25000,]
# screen_name = data.frame(screen_name = user_subset)
# userid = predict_bot(screen_name, token = token)
# 
# write_csv(userid, "user2.csv")

# #25000:50000
# user_subset = df_user[25000:50000]
# userid = lookup_users(user_subset,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user3.csv")
# 
# #50000:75000
# user_subset = df_user[50000:75000]
# userid = lookup_users(user_subset,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user4.csv")
# 
# #75000:100000
# user_subset = df_user[75000:100000]
# userid = lookup_users(user_subset,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user5.csv")
# 
# #100000:150000
# user_subset = df_user[100000:150000]
# userid = lookup_users(user_subset,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user6.csv")
# 
# #150000:202490 ENDE!
# user_subset = df_user[150000:202490]
# userid = lookup_users(user_subset,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user7.csv")



# user_id2 = NA
# 
# for (i in 17182:length(df_user)) {
#   user = get_user_id(df_user[i], bearer_token)
#   user_id2[i] = user}
# 
# user_id2 = as.data.frame(user_id2)
# user_id2 = user_id2 %>% filter(!is.na(user_id2)) 
# user_id2 = user_id2$user_id2
# 
# user_profile2 = get_user_profile(user_id2, bearer_token)
# 
# username2 = as.data.frame(user_profile2$username)
# created2 = as.data.frame(user_profile2$created_at)
# id2 = as.data.frame(user_profile2$id)
# metrics2 = as.data.frame(user_profile2$public_metrics)
# 
# user_profile2 = cbind(username2, created2, id2, metrics2)
# colnames(user_profile2) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile2, "user_profile2.csv")
# 
# options(scipen=999) #avoid scientific notations (e.g. e+18)
# 
# user_id3 = read_csv("user3.csv")
# user_id3 = user_id3 %>% filter(!is.na(user_id3))
# user_id3 = user_id3$userid.user_id
# 
# user_profile3 = get_user_profile(user_id3, bearer_token)
# 
# username3 = as.data.frame(user_profile3$username)
# created3 = as.data.frame(user_profile3$created_at)
# id3 = as.data.frame(user_profile3$id)
# metrics3 = as.data.frame(user_profile3$public_metrics)
# 
# user_profile3 = cbind(username3, created3, id3, metrics3)
# colnames(user_profile3) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile3, "user_profile3.csv")
# 
# user_id4 = read_csv("user4.csv")
# user_id4 = user_id4 %>% filter(!is.na(user_id4))
# user_id4 = user_id4$userid.user_id
# 
# user_profile4 = get_user_profile(user_id4, bearer_token)
# 
# username4 = as.data.frame(user_profile4$username)
# created4 = as.data.frame(user_profile4$created_at)
# id4 = as.data.frame(user_profile4$id)
# metrics4 = as.data.frame(user_profile4$public_metrics)
# 
# user_profile4 = cbind(username4, created4, id4, metrics4)
# colnames(user_profile4) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile4, "user_profile4.csv")
# 
# user_id5 = read_csv("user5.csv")
# user_id5 = user_id5 %>% filter(!is.na(user_id5))
# user_id5 = user_id5$userid.user_id
# 
# user_profile5 = get_user_profile(user_id5, bearer_token)
# 
# username5 = as.data.frame(user_profile5$username)
# created5 = as.data.frame(user_profile5$created_at)
# id5 = as.data.frame(user_profile5$id)
# metrics5 = as.data.frame(user_profile5$public_metrics)
# 
# user_profile5 = cbind(username5, created5, id5, metrics5)
# colnames(user_profile5) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile5, "user_profile5.csv")
# 
# user_id6 = read_csv("user6.csv")
# user_id6 = user_id6 %>% filter(!is.na(user_id6))
# user_id6 = user_id6$userid.user_id
# 
# user_profile6 = get_user_profile(user_id6, bearer_token)
# 
# username6 = as.data.frame(user_profile6$username)
# created6 = as.data.frame(user_profile6$created_at)
# id6 = as.data.frame(user_profile6$id)
# metrics6 = as.data.frame(user_profile6$public_metrics)
# 
# user_profile6 = cbind(username6, created6, id6, metrics6)
# colnames(user_profile6) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_profile6, "user_profile6.csv")
# 
# user_id7 = read_csv("user7.csv")
# user_id7 = user_id7 %>% filter(!is.na(user_id7))
# user_id7 = user_id7$userid.user_id
# 
# user_profile7 = get_user_profile(user_id7, bearer_token)
# 
# username7 = as.data.frame(user_profile7$username)
# created7 = as.data.frame(user_profile7$created_at)
# id7 = as.data.frame(user_profile7$id)
# metrics7 = as.data.frame(user_profile7$public_metrics)
# 
# user_profile7 = cbind(username7, created7, id7, metrics7)
# # colnames(user_profile7) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# user_metdadata$username = tolower(user_metdadata$username) #Replace to lower words
# # 
# # write.csv(user_profile7, "user_profile7.csv")
# 
# 
# ####COMBINE DATAFRAMES####
# 
# userid1 = read_csv("userid/user_profile.csv")
# userid2 = read_csv("userid/user_profile2.csv")
# userid3 = read_csv("userid/user_profile3.csv")
# userid4 = read_csv("userid/user_profile4.csv")
# userid5 = read_csv("userid/user_profile5.csv")
# userid6 = read_csv("userid/user_profile6.csv")
# userid7 = read_csv("userid/user_profile7.csv")
# 
# user_metdadata = rbind(userid1,userid2,userid3,userid4,userid5,userid6,userid7)
# 
# 
# #check missing users
# setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
# df = read_csv("df_nonequal_size")
# 
# df_user = df %>% select(username) 
# df_user = df_user[!duplicated(df_user$username),]
# df_user = df_user %>% left_join(user_metdadata, by = "username")
# df_user$id[is.na(df_user$id)] = "missing"
# df_user = df_user %>% filter(id == "missing")
# df_missing = df_user$username
# 
# userid = lookup_users(df_missing,parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user_missing.csv")
# 
# userid = lookup_users(df_missing[91000:length(df_missing)],parse = TRUE, token = token)
# userid = data.frame(userid$user_id,userid$screen_name)
# 
# write_csv(userid, "user_missing2.csv")
# 
# 
# missing = read_csv("user_missing.csv")
# missing2 = read_csv("user_missing2.csv")
# user_missing = rbind(missing, missing2)
# user_missing = user_missing %>% filter(!is.na(userid.user_id))
# user_missing = user_missing$userid.user_id
# 
# user_missing_profile = get_user_profile(user_missing, bearer_token)
# 
# username = as.data.frame(user_missing_profile$username)
# created = as.data.frame(user_missing_profile$created_at)
# id = as.data.frame(user_missing_profile$id)
# metrics = as.data.frame(user_missing_profile$public_metrics)
# 
# user_missing_profile = cbind(username, created, id, metrics)
# colnames(user_missing_profile) = c("username", "created_at", "id", "followers_count","following_count", "tweet_count","listed_count")
# 
# write.csv(user_missing_profile, "user_missing_profile.csv")

####COMBINE DATAFRAMES####

userid1 = read_csv("userid/user_profile.csv")
userid2 = read_csv("userid/user_profile2.csv")
userid3 = read_csv("userid/user_profile3.csv")
userid4 = read_csv("userid/user_profile4.csv")
userid5 = read_csv("userid/user_profile5.csv")
userid6 = read_csv("userid/user_profile6.csv")
userid7 = read_csv("userid/user_profile7.csv")
userid_missing = read_csv("user_missing_profile.csv")

user_metadata = rbind(userid1,userid2,userid3,userid4,userid5,userid6,userid7, userid_missing)
user_metadata = user_metadata[!duplicated(user_metadata$id),]

# #check missing users
# setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
# df = read_csv("df_nonequal_size")
# 
# df_user = df %>% select(username) 
# df_user = df_user[!duplicated(df_user$username),]
# 
# user_metadata$username = tolower(user_metadata$username) #Replace to lower words
# df_user = df_user %>% left_join(user_metadata,by="username")
# 
# df_user$id[is.na(df_user$id)] = "missing"
# df_user = df_user %>% filter(id == "missing")


user_metadata = user_metadata %>% select(-X1) 


#calculate user_since
user_metadata$today = "2021-12-20 11:08:00 UTC"
user_metadata$user_since = NA
for (i in 1:length(user_metadata$id)) {
  user_metadata$user_since[i] = 
    round(as.numeric(difftime(user_metadata$today[i],user_metadata$created_at[i],units ="days"))/(365.25/12))
} 

user_metadata$username = tolower(user_metadata$username) #Replace to lower words


setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(user_metadata, "user_metadata.csv")

# 
# #GET TWEETS OF ALL USERS
# 
# user_metadata = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/user_metadata.csv")
# usernames = user_metadata$username
# 
# tweets1_10 = get_all_tweets(users = usernames[1:10],
#                             bearer_token = get_bearer(),
#                             start_tweets = "2007-01-01T00:00:00Z",
#                             end_tweets = "2021-12-31T00:00:00Z",
#                             n = 100000000000000000000000000000000000000,
#                             file = "C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/TWEETS/tweets1_10"
# )
# 
# tweets1_100 = readRDS("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/TWEETS/part1")

