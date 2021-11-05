####LIBRARIES####
library(dplyr)
library(tidyverse)

#load datasets
setwd("~/GitHub/twint/outputs")
company_greenwashing =
company_otherkeywords =
company_otherhashtags =

#combine datasets
company = rbind(company_greenwashing, company_otherkeywords, company_otherhashtags)

#remove duplicates
company_unique = company %>%
  distinct()

#extract year from date column
#https://www.marsja.se/how-to-extract-year-from-date-in-r-with-examples/
  
  

