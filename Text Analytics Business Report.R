# Reading necessary documents
library(textreadr)
library(textshape)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(tm)
library(reshape2)
library(wordcloud)
library(ggplot2)
library(igraph)
library(ggraph)

data(stop_words)

# Uploading files
setwd("/Users/jasmijnvanhulsen/Desktop/Classes/Module B/Text Analytics/Google_2")
nm <- list.files(path="/Users/jasmijnvanhulsen/Desktop/Classes/Module B/Text Analytics/Google_2")

# Bind all the documents together
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

# Creating a df with Google and Netflix to compare 
job_all <- data_frame(title=c("Data Analyst","Data Scientist", "Netflix"), text=my_txt_text)

# Creating a df with just Google
job <- job_all[-3 ,]

# Creating a df with just Netflix
job_nf <- job_all[-c(1,2) , ]

#-----------------------------------------------------------------------------#
#                            Tokanization                                     #
#-----------------------------------------------------------------------------#
# Create tokens and anti_join stopwords - Google 
job_tokens <- job %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

job_tokens

# Create tokens with location information
job_tokens_title <- job %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(title, word, sort=TRUE) %>%
  ungroup()

job_tokens_title

# Create tokens with location information including Netflix 
job_tokens_title_all <- job_all %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(title, word, sort=TRUE) %>%
  ungroup()

job_tokens_title_all

# Creating graph with most frequent words - Google 
freq_hist <- job_tokens %>%
  mutate(word=reorder(word, n)) %>%
  filter(n > 45) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#-----------------------------------------------------------------------------#
#                             TF-IDF                                          #
#-----------------------------------------------------------------------------#

# Create total words per article
total_words <- job_tokens_title_all %>%
  group_by(title) %>%
  summarize(total=sum(n))

# Join ai_tidy with total_words
title_words <- left_join(job_tokens_title_all, total_words)

title_words

# Bind TF IDF
title_words <- title_words %>%
  bind_tf_idf(word, title, n)

title_words %>%
  arrange(desc(tf_idf))
  
# Graphing most important words 
title_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(title) %>%
  top_n(10) %>% # adjust for more tokens
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

#-----------------------------------------------------------------------------#
#                              Sentiment                                      #
#-----------------------------------------------------------------------------#

# Sentiment Wordcloud NRC - Google
job_tokens %>%
  inner_join(get_sentiments("nrc")) %>% #lexicon_nrc
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=100, scale=c(1,0.1))

# Sentiment Wordcloud NRC - Netflix
job_nf %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  inner_join(get_sentiments("nrc")) %>% #lexicon_nrc
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=50, scale=c(1,0.5))

# Sentiment Wordcloud Bing - Google 
job_tokens %>%
  inner_join(get_sentiments("bing")) %>% #lexicon_bing
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("Red", "Dark Green"),
                   max.words=200, scale=c(1,1))

# Sentiment Wordcloud Bing - Netflix 
job_nf %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  inner_join(get_sentiments("bing")) %>% #lexicon_bing
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("Red", "Dark Green"),
                   max.words=200, scale=c(1,1))

#-----------------------------------------------------------------------------#
#                                Bigram                                       #
#-----------------------------------------------------------------------------#
# Create bigrams with all words 
job_bigrams <- job_all %>%
  group_by(title) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE) %>%
  ungroup()

# Seperate words in bigrams
bigrams_separated <- job_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Take out stopwords 
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# See the new bigrams
bigram_counts

# Unite words
bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

# Create TF IDF for bigrams
bigram_tf_idf <- bigram_united %>%
  count(title, bigram) %>%
  bind_tf_idf(bigram, title, n) %>%
  arrange(desc(tf_idf))

# Create bigram relationships
bigram_graph <- bigram_counts %>%
  filter(n>2) %>% #less data, so lower n (maybe n=2)
  graph_from_data_frame()

# Graph relationships 
ggraph(bigram_graph, layout = "fr") + # fr for frequency
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Filtering data
bigrams_filtered %>%
  filter(word1 == "data") %>%
  count(title, word2, sort = TRUE)

# Graphing data
bigrams_filtered %>%
  group_by(title) %>%
  filter(word1 == "data") %>%
  top_n(8) %>%
  ungroup %>%
  ggplot(aes(word2, word1, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

# Filtering business
bigrams_filtered %>%
  filter(word1 == "business") %>%
  count(title, word2, sort = TRUE)

# Graphing business
bigrams_filtered %>%
  group_by(title) %>%
  filter(word1 == "business") %>%
  top_n(6) %>% 
  ungroup %>%
  ggplot(aes(word2, word1, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

# Filtering skills 
bigrams_filtered %>%
  filter(word2 == "skills") %>%
  count(title, word1, sort = TRUE)

# Graphing skills
bigrams_filtered %>%
  group_by(title) %>%
  filter(word2 == "skills") %>%
  top_n(8) %>% 
  ungroup %>%
  ggplot(aes(word1, word2, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

#-----------------------------------------------------------------------------#
#                                Quadrogram                                   #
#-----------------------------------------------------------------------------#
# Create quadrogram with all words 
job_quadrogram <- job %>%
  group_by(title) %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  count(quadrogram, sort = TRUE) %>%
  ungroup()

# Seperate words in quadrogram
quadrogram_separated <- job_quadrogram %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep = " ")

# Take out stopwords 
quadrogram_filtered <- quadrogram_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)

# Creating the new quadrogram, "no-stop-words":
quadrogram_counts <- quadrogram_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)

# See the new quadrogram
quadrogram_counts

# Unite words
quadrogram_united <- quadrogram_filtered %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

# Create TF IDF for quadrogram
quadrogram_tf_idf <- quadrogram_united %>%
  count(title, quadrogram) %>%
  bind_tf_idf(quadrogram, title, n) %>%
  arrange(desc(tf_idf))

# Create quadrogram relationships
quadrogram_graph <- quadrogram_counts %>%
  filter(n>1) %>% #less data, so lower n (maybe n=2)
  graph_from_data_frame()

# Graph quadrogram relationships
ggraph(quadrogram_graph, layout = "fr") + # fr for frequency
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Grouping first 3 words together, so I can compare to last word
quadrogram_un_last <- quadrogram_filtered %>%
  unite(quadrogram, word1, word2, word3, sep=" ")

# Grouping last 3 words together, so I can compare to fist word
quadrogram_un_first <- quadrogram_filtered %>%
  unite(quadrogram, word2, word3, word4, sep=" ")

# Check combinations with Product
quadrogram_un_first %>%
  group_by(title) %>%
  filter(word1 == "product") %>%
  top_n(5) %>% # adjust for more tokens
  ungroup %>%
  ggplot(aes(quadrogram, word1, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

# Check combinations with Experience
quadrogram_un_last %>%
  group_by(title) %>%
  filter(word4 == "experience") %>%
  top_n(5) %>% # adjust for more tokens
  ungroup %>%
  ggplot(aes(quadrogram, word4, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()

# Check combinations with Analysis
quadrogram_un_last %>%
  group_by(title) %>%
  filter(word4 == "analysis") %>%
  top_n(5) %>% # adjust for more tokens
  ungroup %>%
  ggplot(aes(quadrogram, word4, fill=title))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~title, ncol=2, scales="free")+
  coord_flip()
