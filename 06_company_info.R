library(readxl)
library(tidyverse)
options(digits = 2) # reduce decimal places
options(scipen=999) #avoid scientific notations (e.g. e+18)

company_info = read_excel("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/company_info/company.xlsx")

company_info_select = company_info  %>% group_by(company) %>% 
  mutate(totalGHG = sum(GHG1,GHG2,GHG3), env_performance = log(1+totalGHG/(revenues/100000000000))) %>% select(-c(GHG1,GHG2,GHG3, 'GHG measure')) 

setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA")
write.csv(company_info_select, "company_info_select.csv")

