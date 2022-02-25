#Libraries####
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(viridis)
library(DT)
library(plotly)


####GREEN ADVERTISING#####
postings_combined = read_excel("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/postings_combined.xlsx")

postings_combined$company[postings_combined$company == "cocacola"] = "Coca Cola"
postings_combined$company[postings_combined$company == "exxonmobil"] = "Exxonmobil"
postings_combined$company[postings_combined$company == "hm"] = "H&M"
postings_combined$company[postings_combined$company == "ikea"] = "IKEA"
postings_combined$company[postings_combined$company == "nestle"] = "Nestle"
postings_combined$company[postings_combined$company == "shell"] = "Shell"
postings_combined$company[postings_combined$company == "unilever"] = "Unilever"
postings_combined$company[postings_combined$company == "vw"] = "Volkswagen"


svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\green_adv_percompany.svg",bg = "transparent")
print(
  postings_combined %>%
    ggplot(aes(x = day, y = total_env_posts_perweek, fill = company))+
    geom_area(alpha=0.5) +
    geom_line(alpha = 0.3) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="none") +
    theme_ipsum() +
    ylab("# Environmental Posts")+
    xlab("Day") +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10),
      plot.title = element_text(size=10), 
      text=element_text(size=8,  family="sans")
    ) +
    facet_wrap(~company, nrow = 2)
)
dev.off()

postings_boolean = postings_combined %>% group_by(company) %>% count(env_claim_made)
postings_boolean$Advertising = NA

postings_boolean$Advertising[postings_boolean$env_claim_made == 1] = "Green Advertising"
postings_boolean$Advertising[postings_boolean$env_claim_made == 0] = "No/General Advertising"

svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\green_adv_percompany_boolean.svg",bg = "transparent")
print(
postings_boolean %>%
ggplot(aes(x="", y=n, fill=Advertising)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme(legend.position="none") +
  theme_void() +
  xlab("Count") +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=10), 
    text=element_text(size=8,  family="sans")
  ) +
  facet_wrap(~company, nrow = 2)
)
dev.off()


###NEGATIVE WOM#####
df_vice_combined = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_vice_combined.csv")


df_vice_combined$company[df_vice_combined$company == "cocacola"] = "Coca Cola"
df_vice_combined$company[df_vice_combined$company == "exxonmobil"] = "Exxonmobil"
df_vice_combined$company[df_vice_combined$company == "hm"] = "H&M"
df_vice_combined$company[df_vice_combined$company == "ikea"] = "IKEA"
df_vice_combined$company[df_vice_combined$company == "nestle"] = "Nestle"
df_vice_combined$company[df_vice_combined$company == "shell"] = "Shell"
df_vice_combined$company[df_vice_combined$company == "unilever"] = "Unilever"
df_vice_combined$company[df_vice_combined$company == "vw"] = "Volkswagen"

vw = df_vice_combined %>% filter(company == "Volkswagen") %>%
  filter(date > "2015-09-16" & date < "2016-09-16") %>%
  mutate(day = row_number())

cocacola = df_vice_combined %>% filter(company == "Coca Cola")  %>% 
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(day = row_number())

nestle = df_vice_combined %>% filter(company == "Nestle") %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(day = row_number())

exxon = df_vice_combined %>% filter(company == "Exxonmobil") %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(day = row_number())


unilever = df_vice_combined %>% filter(company == "Unilever") %>%
  filter(date > "2020-11-01" & date < "2021-11-01") %>%
  mutate(day = row_number())

ikea = df_vice_combined %>% filter(company == "IKEA") %>%
  filter(date > "2020-08-21" & date < "2021-08-21") %>%
  mutate(day = row_number())

hm = df_vice_combined %>% filter(company == "H&M") %>%
  filter(date > "2020-08-23" & date < "2021-08-23") %>%
  mutate(day = row_number())


shell = df_vice_combined %>% filter(company == "Shell") %>%
  filter(date > "2020-11-01" & date < "2021-11-02") %>%
  mutate(day = row_number()) 

concerns_combined = rbind(cocacola , exxon , hm , ikea , shell , unilever , vw , nestle)


svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\negativeWOM_percompany.svg",bg = "transparent")
print(
  concerns_combined %>%
    ggplot(aes(x = day, y = n, fill = company))+
    geom_area(alpha=0.5) +
    geom_line(alpha = 0.3) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="none") +
    theme_ipsum() +
    ylab("#Tweets with Moral Concerns")+
    xlab("Day") +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10),
      plot.title = element_text(size=10), 
      text=element_text(size=8,  family="sans")
    ) +
    facet_wrap(~company, nrow = 2, scales = "free")
)
dev.off()


###MFT####
df_select = read_csv("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/df_select.csv")

MFTpercompany = df_select %>% select(date, company, vice_sum) %>%
  group_by(company, date) %>% summarise(vice_mean = mean(vice_sum))

MFTpercompany$day = c(1:364,1:364,1:364,1:364,1:364,1:364,1:364,1:365)

MFTpercompany$company[MFTpercompany$company == "cocacola"] = "Coca Cola"
MFTpercompany$company[MFTpercompany$company == "exxonmobil"] = "Exxonmobil"
MFTpercompany$company[MFTpercompany$company == "hm"] = "H&M"
MFTpercompany$company[MFTpercompany$company == "ikea"] = "IKEA"
MFTpercompany$company[MFTpercompany$company == "nestle"] = "Nestle"
MFTpercompany$company[MFTpercompany$company == "shell"] = "Shell"
MFTpercompany$company[MFTpercompany$company == "unilever"] = "Unilever"
MFTpercompany$company[MFTpercompany$company == "vw"] = "Volkswagen"


svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\MFT_percompany.svg",bg = "transparent")
print(MFTpercompany %>%
  ggplot( aes(x=day, y=vice_mean, group=company, fill=company)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ylab("Average Amount of Moral Outrage")+
  xlab("Day") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~company, nrow = 2))
dev.off()

####COUNT TWEETS####

df_select_amount = df_select %>% select(company, date, id)

df_select_amount$company[df_select_amount$company == "cocacola"] = "Coca Cola"
df_select_amount$company[df_select_amount$company == "exxonmobil"] = "Exxonmobil"
df_select_amount$company[df_select_amount$company == "hm"] = "H&M"
df_select_amount$company[df_select_amount$company == "ikea"] = "IKEA"
df_select_amount$company[df_select_amount$company == "nestle"] = "Nestle"
df_select_amount$company[df_select_amount$company == "shell"] = "Shell"
df_select_amount$company[df_select_amount$company == "unilever"] = "Unilever"
df_select_amount$company[df_select_amount$company == "vw"] = "Volkswagen"


df_select_amount = df_select_amount %>% group_by(date, company) %>% count()

df_select_amount$day = c(1:364,1:364,1:364,1:364,1:364,1:364,1:364,1:365)

svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\Tweet_Count.svg",bg = "transparent")
print(
  ggplot(df_select_amount, aes(x = day, y = n, fill = company)) +
  geom_line(color = "#69b3a2", size = .5) +
  ylab("#Tweets")+ xlab("Day") + 
  theme_ipsum()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=10), 
    text=element_text(size=8,family="sans")
  ) +
  facet_wrap(~company, nrow = 2, scales = "free") 
)
dev.off()



###TOPICS####
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/subset_lda.RData")
subset_lda = lda
# #Beta probabilities for each word
topicprob = as.matrix(subset_lda@gamma)
word_topicprob = tidytext::tidy(lda, matrix = "beta")
# 
top_terms_per_topic = word_topicprob %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)
svg("C:\\Users\\eva_s\\OneDrive\\MASTER\\5. Semester_THESIS\\Data Analytics\\descriptives\\topic_distribution.svg",bg = "transparent")
print(
top_terms_per_topic %>%
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=10), 
    text=element_text(size=8,family="sans")
  )+
  tidytext::scale_y_reordered()
)
dev.off()



