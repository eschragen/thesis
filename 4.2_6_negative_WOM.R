library(readr)
library(tidyverse)
library(data.table)
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/data/data_breakingpoints")
emfd = read_csv("results_emfd.csv")
emfd = emfd %>% group_by(id) %>% mutate(vice_sum = sum(loyalty.vice, care.vice, fairness.vice, sanctity.vice, authority.vice)) 
df = read_csv("df_nonequal_size")
df = df %>% left_join(emfd, by = "id")

#filter sum of vice so that tweet contains at least some moral outrage
df_vice = df %>% select(id, company, vice_sum,date) %>% filter(vice_sum > 0)
years = read_csv("year2015_2021.csv")

df_vice_cocacola = df_vice %>% filter(company == "cocacola")%>% group_by(date) %>% count()
df_vice_cocacola = years %>% left_join(df_vice_cocacola, by = "date") %>% mutate(company = "cocacola")
df_vice_cocacola$n[is.na(df_vice_cocacola$n)] = 0
setDT(df_vice_cocacola)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_cocacola$vice_virality[i] = sum(df_vice_cocacola$n[1:i])
  i = i +1
}

df_vice_cocacola_valence = df_vice %>% filter(company == "cocacola")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_cocacola_valence = years %>% left_join(df_vice_cocacola_valence, by = "date") 
df_vice_cocacola_valence$mean[is.na(df_vice_cocacola_valence$mean)] = 0
setDT(df_vice_cocacola_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_cocacola_valence$vice_valence[i] = mean(df_vice_cocacola_valence$mean[1:i])
  i = i +1
}

df_vice_cocacola = df_vice_cocacola %>% left_join(df_vice_cocacola_valence, by = "date")
df_vice_cocacola = df_vice_cocacola %>% select(-c(n,mean))

df_vice_exxonmobil = df_vice %>% filter(company == "exxonmobil")%>% group_by(date) %>% count()
df_vice_exxonmobil = years %>% left_join(df_vice_exxonmobil, by = "date") %>% mutate(company = "exxonmobil")
df_vice_exxonmobil$n[is.na(df_vice_exxonmobil$n)] = 0
setDT(df_vice_exxonmobil)[, vice_virality := frollmean(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_exxonmobil$vice_virality[i] = sum(df_vice_exxonmobil$n[1:i])
  i = i +1
}

df_vice_exxonmobil_valence = df_vice %>% filter(company == "exxonmobil")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_exxonmobil_valence = years %>% left_join(df_vice_exxonmobil_valence, by = "date") 
df_vice_exxonmobil_valence$mean[is.na(df_vice_exxonmobil_valence$mean)] = 0
setDT(df_vice_exxonmobil_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_exxonmobil_valence$vice_valence[i] = mean(df_vice_exxonmobil_valence$mean[1:i])
  i = i +1
}

df_vice_exxonmobil = df_vice_exxonmobil %>% left_join(df_vice_exxonmobil_valence, by = "date")
df_vice_exxonmobil = df_vice_exxonmobil %>% select(-c(n,mean))

df_vice_ikea = df_vice %>% filter(company == "ikea")%>% group_by(date) %>% count()
df_vice_ikea = years %>% left_join(df_vice_ikea, by = "date") %>% mutate(company = "ikea")
df_vice_ikea$n[is.na(df_vice_ikea$n)] = 0
setDT(df_vice_ikea)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_ikea$vice_virality[i] = sum(df_vice_ikea$n[1:i])
  i = i +1
}

df_vice_ikea_valence = df_vice %>% filter(company == "ikea")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_ikea_valence = years %>% left_join(df_vice_ikea_valence, by = "date") 
df_vice_ikea_valence$mean[is.na(df_vice_ikea_valence$mean)] = 0
setDT(df_vice_ikea_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_ikea_valence$vice_valence[i] = mean(df_vice_ikea_valence$mean[1:i])
  i = i +1
}

df_vice_ikea = df_vice_ikea %>% left_join(df_vice_ikea_valence, by = "date")
df_vice_ikea = df_vice_ikea %>% select(-c(n,mean))

df_vice_nestle = df_vice %>% filter(company == "nestle")%>% group_by(date) %>% count()
df_vice_nestle = years %>% left_join(df_vice_nestle, by = "date") %>% mutate(company = "nestle")
df_vice_nestle$n[is.na(df_vice_nestle$n)] = 0
setDT(df_vice_nestle)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_nestle$vice_virality[i] = sum(df_vice_nestle$n[1:i])
  i = i +1
}

df_vice_nestle_valence = df_vice %>% filter(company == "nestle")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_nestle_valence = years %>% left_join(df_vice_nestle_valence, by = "date") 
df_vice_nestle_valence$mean[is.na(df_vice_nestle_valence$mean)] = 0
setDT(df_vice_nestle_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_nestle_valence$vice_valence[i] = mean(df_vice_nestle_valence$mean[1:i])
  i = i +1
}

df_vice_nestle = df_vice_nestle %>% left_join(df_vice_nestle_valence, by = "date")
df_vice_nestle = df_vice_nestle %>% select(-c(n,mean))

df_vice_shell = df_vice %>% filter(company == "shell")%>% group_by(date) %>% count()
df_vice_shell = years %>% left_join(df_vice_shell, by = "date") %>% mutate(company = "shell")
df_vice_shell$n[is.na(df_vice_shell$n)] = 0
setDT(df_vice_shell)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_shell$vice_virality[i] = sum(df_vice_shell$n[1:i])
  i = i +1
}

df_vice_shell_valence = df_vice %>% filter(company == "shell")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_shell_valence = years %>% left_join(df_vice_shell_valence, by = "date") 
df_vice_shell_valence$mean[is.na(df_vice_shell_valence$mean)] = 0
setDT(df_vice_shell_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_shell_valence$vice_valence[i] = mean(df_vice_shell_valence$mean[1:i])
  i = i +1
}

df_vice_shell = df_vice_shell %>% left_join(df_vice_shell_valence, by = "date")
df_vice_shell = df_vice_shell %>% select(-c(n,mean))


df_vice_vw = df_vice %>% filter(company == "vw")%>% group_by(date) %>% count()
df_vice_vw = years %>% left_join(df_vice_vw, by = "date") %>% mutate(company = "vw")
df_vice_vw$n[is.na(df_vice_vw$n)] = 0
setDT(df_vice_vw)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_vw$vice_virality[i] = sum(df_vice_vw$n[1:i])
  i = i +1
}

df_vice_vw_valence = df_vice %>% filter(company == "vw")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_vw_valence = years %>% left_join(df_vice_vw_valence, by = "date") 
df_vice_vw_valence$mean[is.na(df_vice_vw_valence$mean)] = 0
setDT(df_vice_vw_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_vw_valence$vice_valence[i] = mean(df_vice_vw_valence$mean[1:i])
  i = i +1
}

df_vice_vw = df_vice_vw %>% left_join(df_vice_vw_valence, by = "date")
df_vice_vw = df_vice_vw %>% select(-c(n,mean))


df_vice_hm = df_vice %>% filter(company == "hm")%>% group_by(date) %>% count()
df_vice_hm = years %>% left_join(df_vice_hm, by = "date") %>% mutate(company = "hm")
df_vice_hm$n[is.na(df_vice_hm$n)] = 0
setDT(df_vice_hm)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_hm$vice_virality[i] = sum(df_vice_hm$n[1:i])
  i = i +1
}

df_vice_hm_valence = df_vice %>% filter(company == "hm")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_hm_valence = years %>% left_join(df_vice_hm_valence, by = "date") 
df_vice_hm_valence$mean[is.na(df_vice_hm_valence$mean)] = 0
setDT(df_vice_hm_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_hm_valence$vice_valence[i] = mean(df_vice_hm_valence$mean[1:i])
  i = i +1
}

df_vice_hm = df_vice_hm %>% left_join(df_vice_hm_valence, by = "date")
df_vice_hm = df_vice_hm %>% select(-c(n,mean))


df_vice_unilever = df_vice %>% filter(company == "unilever")%>% group_by(date) %>% count()
df_vice_unilever = years %>% left_join(df_vice_unilever, by = "date") %>% mutate(company = "unilever")
df_vice_unilever$n[is.na(df_vice_unilever$n)] = 0
setDT(df_vice_unilever)[, vice_virality := frollsum(n, 7)]
i = 1
for (i in 1:6) {
  df_vice_unilever$vice_virality[i] = sum(df_vice_unilever$n[1:i])
  i = i +1
}

df_vice_unilever_valence = df_vice %>% filter(company == "unilever")%>% group_by(date) %>% summarize(mean = mean(vice_sum))
df_vice_unilever_valence = years %>% left_join(df_vice_unilever_valence, by = "date") 
df_vice_unilever_valence$mean[is.na(df_vice_unilever_valence$mean)] = 0
setDT(df_vice_unilever_valence)[, vice_valence := frollmean(mean, 7)]
i = 1
for (i in 1:6) {
  df_vice_unilever_valence$vice_valence[i] = mean(df_vice_unilever_valence$mean[1:i])
  i = i +1
}

df_vice_unilever = df_vice_unilever %>% left_join(df_vice_unilever_valence, by = "date")
df_vice_unilever = df_vice_unilever %>% select(-c(n,mean))


df_vice_combined = rbind(df_vice_cocacola,df_vice_exxonmobil,df_vice_hm,df_vice_ikea, df_vice_nestle, df_vice_shell, df_vice_unilever, df_vice_vw)
write.csv(df_vice_combined, "df_negativeWOM.csv")
