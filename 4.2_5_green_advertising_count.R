library(readxl)
library(tidyverse)
library(data.table)
library(writexl)
library(ggplot2)
library(hrbrthemes)

####import data####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
years = read_csv("year2015_2021.csv")
claims = read_excel("company_profiles.xlsx")

df = claims %>% select(date, company, environment) 
df$posting = 1
df$environment[is.na(df$environment)] = 0
df$environment = as.numeric(df$environment)
df$posting = as.numeric(df$posting)

####count weekly green advertising for every company seperately####
####cocacola####
#create subset of company
df_cocacola = df %>% filter(company == "cocacola")
#amount of total postings & environmental postings
df_cocacola = df_cocacola %>% group_by(date) %>% 
                      summarise(env_post = sum(environment), total_post = sum(posting)) 
df_cocacola = years %>% left_join(df_cocacola, by = "date")  %>% filter(date > "2020-10-01" & date < "2021-11-01")
df_cocacola$env_post[is.na(df_cocacola$env_post)] = 0
df_cocacola$total_post[is.na(df_cocacola$total_post)] = 0
#amount of postings in the last 7 days
setDT(df_cocacola)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_cocacola)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_cocacola$total_posts_perweek[i] = sum(df_cocacola$total_post[1:i])
  df_cocacola$total_env_posts_perweek[i] = sum(df_cocacola$env_post[1:i])
  i = i +1
}

#calculate ratio of environmental vs. non-environmental postings
df_cocacola = df_cocacola %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_cocacola$ratio_env_nonenv[df_cocacola$total_env_posts_perweek == 0] = 0
df_cocacola$env_claim_made[df_cocacola$total_env_posts_perweek == 0] = 0
df_cocacola$env_claim_made[df_cocacola$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_cocacola = df_cocacola[32:396,]
df_cocacola = df_cocacola %>% mutate(day = row_number(), company = "cocacola")


####nestle####
#create subset of company
df_nestle = df %>% filter(company == "nestle")
#amount of total postings & environmental postings
df_nestle = df_nestle %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_nestle = years %>% left_join(df_nestle, by = "date")  %>% filter(date > "2020-10-01" & date < "2021-11-01")
df_nestle$env_post[is.na(df_nestle$env_post)] = 0
df_nestle$total_post[is.na(df_nestle$total_post)] = 0
#amount of postings in the last 7 days
setDT(df_nestle)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_nestle)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_nestle$total_posts_perweek[i] = sum(df_nestle$total_post[1:i])
  df_nestle$total_env_posts_perweek[i] = sum(df_nestle$env_post[1:i])
  i = i +1
}

#calculate ratio of environmental vs. non-environmental postings
df_nestle = df_nestle %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_nestle$ratio_env_nonenv[df_nestle$total_env_posts_perweek == 0] = 0
df_nestle$env_claim_made[df_nestle$total_env_posts_perweek == 0] = 0
df_nestle$env_claim_made[df_nestle$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_nestle = df_nestle[32:396,]
df_nestle = df_nestle %>% mutate(day = row_number(), company = "nestle")


####ikea####
#create subset of company
df_ikea = df %>% filter(company == "ikea")
#amount of total postings & environmental postings
df_ikea = df_ikea %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_ikea = years %>% left_join(df_ikea, by = "date")  %>% filter(date > "2020-07-21" & date < "2021-08-21") 
df_ikea$env_post[is.na(df_ikea$env_post)] = 0
df_ikea$total_post[is.na(df_ikea$total_post)] = 0
df_ikea$total_env_media[is.na(df_ikea$total_env_media)] = 0
#amount of postings in the last 7 days
setDT(df_ikea)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_ikea)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_ikea$total_posts_perweek[i] = sum(df_ikea$total_post[1:i])
  df_ikea$total_env_posts_perweek[i] = sum(df_ikea$env_post[1:i])
  i = i +1
}


#calculate ratio of environmental vs. non-environmental postings
df_ikea = df_ikea %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_ikea$ratio_env_nonenv[df_ikea$total_env_posts_perweek == 0] = 0
df_ikea$env_claim_made[df_ikea$total_env_posts_perweek == 0] = 0
df_ikea$env_claim_made[df_ikea$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_ikea = df_ikea[32:396,]
df_ikea = df_ikea %>% mutate(day = row_number(), company = "ikea")


####exxonmobil####
#create subset of company
df_exxonmobil = df %>% filter(company == "exxonmobil")
#amount of total postings & environmental postings
df_exxonmobil = df_exxonmobil %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_exxonmobil = years %>% left_join(df_exxonmobil, by = "date") %>% filter(date > "2020-10-01" & date < "2021-11-01")
df_exxonmobil$env_post[is.na(df_exxonmobil$env_post)] = 0
df_exxonmobil$total_post[is.na(df_exxonmobil$total_post)] = 0
#amount of postings in the last 7 days
setDT(df_exxonmobil)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_exxonmobil)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_exxonmobil$total_posts_perweek[i] = sum(df_exxonmobil$total_post[1:i])
  df_exxonmobil$total_env_posts_perweek[i] = sum(df_exxonmobil$env_post[1:i])
  df_exxonmobil$total_env_media_perweek[i] = sum(df_exxonmobil$total_env_media[1:i])
  i = i +1
}

#calculate ratio of environmental vs. non-environmental postings
df_exxonmobil = df_exxonmobil %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_exxonmobil$ratio_env_nonenv[df_exxonmobil$total_env_posts_perweek == 0] = 0
df_exxonmobil$env_claim_made[df_exxonmobil$total_env_posts_perweek == 0] = 0
df_exxonmobil$env_claim_made[df_exxonmobil$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_exxonmobil = df_exxonmobil[32:396,]
df_exxonmobil = df_exxonmobil %>% mutate(day = row_number(), company = "exxonmobil")


####hm####
#create subset of company
df_hm = df %>% filter(company == "hm")
#amount of total postings & environmental postings
df_hm = df_hm %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_hm = years %>% left_join(df_hm, by = "date") %>% filter(date > "2020-07-23" & date < "2021-08-23") 
df_hm$env_post[is.na(df_hm$env_post)] = 0
df_hm$total_post[is.na(df_hm$total_post)] = 0
#amount of postings in the last 7 days
setDT(df_hm)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_hm)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_hm$total_posts_perweek[i] = sum(df_hm$total_post[1:i])
  df_hm$total_env_posts_perweek[i] = sum(df_hm$env_post[1:i])
  df_hm$total_env_media_perweek[i] = sum(df_hm$total_env_media[1:i])
  i = i +1
}


#calculate ratio of environmental vs. non-environmental postings
df_hm = df_hm %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_hm$ratio_env_nonenv[df_hm$total_env_posts_perweek == 0] = 0
df_hm$env_claim_made[df_hm$total_env_posts_perweek == 0] = 0
df_hm$env_claim_made[df_hm$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_hm = df_hm[32:396,]
df_hm = df_hm %>% mutate(day = row_number(), company = "hm")



####shell####
#create subset of company
df_shell = df %>% filter(company == "shell")
#amount of total postings & environmental postings
df_shell = df_shell %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_shell = years %>% left_join(df_shell, by = "date")  %>% filter(date > "2020-10-01" & date < "2021-11-01")
df_shell$env_post[is.na(df_shell$env_post)] = 0
df_shell$total_post[is.na(df_shell$total_post)] = 0
df_shell$total_env_media[is.na(df_shell$total_env_media)] = 0
#amount of postings in the last 7 days
setDT(df_shell)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_shell)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_shell$total_posts_perweek[i] = sum(df_shell$total_post[1:i])
  df_shell$total_env_posts_perweek[i] = sum(df_shell$env_post[1:i])
  i = i +1
}



#calculate ratio of environmental vs. non-environmental postings
df_shell = df_shell %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_shell$ratio_env_nonenv[df_shell$total_env_posts_perweek == 0] = 0
df_shell$env_claim_made[df_shell$total_env_posts_perweek == 0] = 0
df_shell$env_claim_made[df_shell$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_shell = df_shell[32:396,]
df_shell = df_shell %>% mutate(day = row_number(), company = "shell")



####unilever####
#create subset of company
df_unilever = df %>% filter(company == "unilever")
#amount of total postings & environmental postings
df_unilever = df_unilever %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_unilever = years %>% left_join(df_unilever, by = "date")  %>% filter(date > "2020-10-01" & date < "2021-11-01")
df_unilever$env_post[is.na(df_unilever$env_post)] = 0
df_unilever$total_post[is.na(df_unilever$total_post)] = 0
df_unilever$total_env_media[is.na(df_unilever$total_env_media)] = 0
#amount of postings in the last 7 days
setDT(df_unilever)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_unilever)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_unilever$total_posts_perweek[i] = sum(df_unilever$total_post[1:i])
  df_unilever$total_env_posts_perweek[i] = sum(df_unilever$env_post[1:i])
  i = i +1
}


#calculate ratio of environmental vs. non-environmental postings
df_unilever = df_unilever %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_unilever$ratio_env_nonenv[df_unilever$total_env_posts_perweek == 0] = 0
df_unilever$env_claim_made[df_unilever$total_env_posts_perweek == 0] = 0
df_unilever$env_claim_made[df_unilever$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_unilever = df_unilever[32:396,]
df_unilever = df_unilever %>% mutate(day = row_number(), company = "unilever")


####vw####
#create subset of company
df_vw = df %>% filter(company == "vw")
#amount of total postings & environmental postings
df_vw = df_vw %>% group_by(date) %>% 
  summarise(env_post = sum(environment), total_post = sum(posting)) 
df_vw = years %>% left_join(df_vw, by = "date")  %>% filter(date > "2015-08-16" & date < "2016-09-16")
df_vw$env_post[is.na(df_vw$env_post)] = 0
df_vw$total_post[is.na(df_vw$total_post)] = 0
df_vw$total_env_media[is.na(df_vw$total_env_media)] = 0
#amount of postings in the last 7 days
setDT(df_vw)[, total_posts_perweek := frollsum(total_post, 7)]
setDT(df_vw)[, total_env_posts_perweek := frollsum(env_post, 7)]

i = 1
for (i in 1:6) {
  df_vw$total_posts_perweek[i] = sum(df_vw$total_post[1:i])
  df_vw$total_env_posts_perweek[i] = sum(df_vw$env_post[1:i])
  df_vw$total_env_media_perweek[i] = sum(df_vw$total_env_media[1:i])
  i = i +1
}


#calculate ratio of environmental vs. non-environmental postings
df_vw = df_vw %>% mutate(ratio_env_nonenv = total_env_posts_perweek/total_posts_perweek) %>%
  select(date, total_env_posts_perweek, ratio_env_nonenv, ratio_env_media)
df_vw$ratio_env_nonenv[df_vw$total_env_posts_perweek == 0] = 0
df_vw$env_claim_made[df_vw$total_env_posts_perweek == 0] = 0
df_vw$env_claim_made[df_vw$total_env_posts_perweek != 0] = 1

#subset of year under consdayeration & add row numbers
df_vw = df_vw[32:396,]
df_vw = df_vw %>% mutate(day = row_number(), company = "vw")

postings_combined = rbind(df_nestle, df_cocacola, df_ikea, df_exxonmobil, df_hm, df_shell, df_unilever, df_vw)

# ####VISUALIZATION####
# total_env_combined = postings_combined %>%  ggplot(aes(x=day, y=total_env_posts_perweek, group=company, color=company)) +
#   geom_line(lwd = .8) + ylab("Count") + xlab("Day")
# total_env_combined
# ratio_env_combined = postings_combined %>%  ggplot(aes(x=day, y=ratio_env_nonenv, group=company, color=company)) +
#   geom_line(lwd = .3) + ylab("Ratio") + xlab("Day")
# ratio_env_combined

####create df####
write_xlsx(x = postings_combined, path = "postings_combined.xlsx", col_names = TRUE)
