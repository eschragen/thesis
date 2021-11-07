#libraries
library(tm)
library(tidyverse)

source()

#prepare data for topic modeling: document-term matrix
dtm = DocumentTermMatrix(tweets_corpus_clean)

#show most frequent words
freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

plot = data.frame(words = names(freq), count = freq)
plot = subset(plot, plot$count > 30) #creating a subset of words having more than 100 frequency
ggplot(data = plot, aes(words, count)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 150 times')+coord_flip()