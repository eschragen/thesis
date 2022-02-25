library(tidyverse)

#Import Company Data
source("~/GitHub/thesis/06_company_info.R")

#Run MFT Scoring manually

#Create df
df_company = df_subset %>% left_join(company_info, by = "company") %>% select(MFT_score, company, employees, revenues, total_assets, total_GHG, industry1, industry2) %>% drop_na()

model = lm(MFT_score ~ total_GHG + industry1 + revenues, data = df_company)
summary(model)

