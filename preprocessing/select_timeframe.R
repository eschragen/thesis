####LIBRARIES####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

#load datasets
setwd("~/GitHub/twint/outputs")

#determine year of analysis with greenwashing df
company = read_csv("hm_greenwashing.csv")

#vw: 2015-09-16 until 2016-09-16
#starbucks: 2018-07-08 until 2019-07-08
#hm: 2020-08-23 until 2021-08-23
#ikea: 2020-08-21 until 2021-08-21

#2020-11-01 until 2021-11-01
#cocacola
#shell
#unilever
#nestle
#mcdonalds
#exxonmobil

tweetfrequencies = company %>% group_by(date) %>% count() 

years = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/year2015_2021.csv")
tweetsperday = years %>% left_join(tweetfrequencies, by = "date") 
tweetsperday[is.na(tweetsperday)] = 0

setDT(tweetsperday)[, sums := frollsum(n, 365)]

ggplot(tweetsperday, aes(x = date, y = n)) + ggtitle("Tweet Frequencies") + geom_line(color = "#69b3a2", size = 1) +
  theme_ipsum()

#Which year period yields the most tweets?
tweetsperday %>% filter(sums == max(sums, na.rm =T))
