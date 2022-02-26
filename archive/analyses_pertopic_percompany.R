####TERM DISTRIBUTIONS#####
load("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company/findtopicnumber_unilever.RData")

word_topicprob = tidytext::tidy(lda, matrix = "beta")

top_terms_per_topic = word_topicprob %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)

# top_terms_per_topic %>% 
#   mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   tidytext::scale_y_reordered()

top_terms_per_topic$topic[top_terms_per_topic$topic == 1] = "Topic 1: Resource Utilization"
top_terms_per_topic$topic[top_terms_per_topic$topic == 2] = "Topic 2: Corporate Greed"
top_terms_per_topic$topic[top_terms_per_topic$topic == 3] = "Topic 3: Call to Action"
top_terms_per_topic$topic[top_terms_per_topic$topic == 4] = "Topic 4: Future Impact"

top_terms_per_topic %>% 
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  xlab("Beta Value") + ylab("Term")+
  scale_x_continuous(breaks = seq(0, 0.06, 0.02)) +
  facet_wrap(~ topic, scale = "free_y") +
  theme(strip.text.x = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


####MEAN DIFFERENCES MORAL OUTRAGE####
setwd("C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/topicmodeling_per_company")
df_new2 = read_csv("df_new2.csv")

cocacola = df_new2 %>% filter(company == "cocacola")
topics_cocacola = read_csv("topics_cocacola.csv")
topics_cocacola = topics_cocacola %>% select(-X1)
colnames(topics_cocacola) = c("id","topic_individual")
cocacola = cocacola %>% left_join(topics_cocacola, by = "id")
cocacola$topic_individual = as.factor(cocacola$topic_individual)

vw = df_new2 %>% filter(company == "vw")
topics_vw = read_csv("topics_vw.csv")
topics_vw = topics_vw %>% select(-X1)
colnames(topics_vw) = c("id","topic_individual")
vw = vw %>% left_join(topics_vw, by = "id")
vw$topic_individual = as.factor(vw$topic_individual)

exxonmobil = df_new2 %>% filter(company == "exxonmobil")
topics_exxonmobil = read_csv("topics_exxonmobil.csv")
topics_exxonmobil = topics_exxonmobil %>% select(-X1)
colnames(topics_exxonmobil) = c("id","topic_individual")
exxonmobil = exxonmobil %>% left_join(topics_exxonmobil, by = "id")
exxonmobil$topic_individual = as.factor(exxonmobil$topic_individual)

shell = df_new2 %>% filter(company == "shell")
topics_shell = read_csv("topics_shell.csv")
topics_shell = topics_shell %>% select(-X1)
colnames(topics_shell) = c("id","topic_individual")
shell = shell %>% left_join(topics_shell, by = "id")
shell$topic_individual = as.factor(shell$topic_individual)

nestle = df_new2 %>% filter(company == "nestle")
topics_nestle = read_csv("topics_nestle.csv")
topics_nestle = topics_nestle %>% select(-X1)
colnames(topics_nestle) = c("id","topic_individual")
nestle = nestle %>% left_join(topics_nestle, by = "id")
nestle$topic_individual = as.factor(nestle$topic_individual)

hm = df_new2 %>% filter(company == "hm")
topics_hm = read_csv("topics_hm.csv")
topics_hm = topics_hm %>% select(-X1)
colnames(topics_hm) = c("id","topic_individual")
hm = hm %>% left_join(topics_hm, by = "id")
hm$topic_individual = as.factor(hm$topic_individual)

unilever = df_new2 %>% filter(company == "unilever")
topics_unilever = read_csv("topics_unilever.csv")
topics_unilever = topics_unilever %>% select(-X1)
colnames(topics_unilever) = c("id","topic_individual")
unilever = unilever %>% left_join(topics_unilever, by = "id")
unilever$topic_individual = as.factor(unilever$topic_individual)

ikea = df_new2 %>% filter(company == "ikea")
topics_ikea = read_csv("topics_ikea.csv")
topics_ikea = topics_ikea %>% select(-X1)
colnames(topics_ikea) = c("id","topic_individual")
ikea = ikea %>% left_join(topics_ikea, by = "id")
ikea$topic_individual = as.factor(ikea$topic_individual)


means_topic_ikea = ikea %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_ikea = ikea %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics IKEA") + 
  stat_summary(fun="mean", size = .3)+ 
 # geom_text(size = 3.5, data = means_topic_ikea, aes(label = round(mean,2), y = mean + 0.2))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_unilever = unilever %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_unilever = unilever %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Unilever") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_unilever, aes(label = round(mean,2), y = mean - 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_cocacola = cocacola %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_cocacola = cocacola %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Coca Cola") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_cocacola, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_exxonmobil = exxonmobil %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_exxonmobil = exxonmobil %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Exxonmobil") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_exxonmobil, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_hm = hm %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_hm = hm %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics H&M") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_hm, aes(label = round(mean,2), y = mean + 0.6))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_vw = vw %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_vw = vw %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics VW") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_vw, aes(label = round(mean,2), y = mean + 0.4))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_nestle = nestle %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_nestle = nestle %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Nestle") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_nestle, aes(label = round(mean,2), y = mean + 0.3))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

means_topic_shell = shell %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% group_by(topic_individual) %>% 
  summarise(mean = mean(moral_outrage))
topic_plot_shell = shell %>% filter(moral_outrage <= quantile(moral_outrage, 0.99)) %>% drop_na(topic_individual) %>%
  ggplot(aes(y = moral_outrage, x = topic_individual)) + 
  geom_boxplot(fill = "grey", outlier.shape = NA, fatten = 0.1) + xlab("") + ylab("Moral Outrage") +
  theme_bw() + ggtitle("Topics Shell") + 
  stat_summary(fun="mean", size = .3)+ 
  #geom_text(size = 3.5, data = means_topic_shell, aes(label = round(mean,2), y = mean + 0.4))+
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

library(patchwork)
topic_plot_cocacola + topic_plot_exxonmobil + topic_plot_hm + topic_plot_ikea +
  topic_plot_nestle + topic_plot_shell + topic_plot_unilever + topic_plot_vw + plot_layout(nrow = 4)


means_topic_cocacola
means_topic_exxonmobil
means_topic_hm
means_topic_ikea
means_topic_nestle
means_topic_shell
means_topic_unilever
means_topic_vw