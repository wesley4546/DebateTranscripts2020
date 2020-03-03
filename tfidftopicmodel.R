library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)

transripts <- read.csv("Democratic Debate 2020 transcripts/debate_transcripts_v3_2020-02-26.csv", stringsAsFactors = FALSE, encoding = "UTF-8")


#Bernie's Transcripts
bern_trans <- 
  transripts %>% 
  as_tibble() %>% 
  filter(speaker == c("Bernie Sanders"))

#Grouping the transription by debate
bern_trans %>% 
  group_by(debate_name)

#Creating my own stopwords
my_stop_words <- tibble::tribble(
  ~word,        ~lexicon,
  "america",    "custom",
  "american",   "custom",
  "people",     "custom",
  "country",    "custom",
  "bring",      "custom",
  "'",          "custom",
  "don",        "custom",
  "ve",         "custom",
)

#Adding my own stopwords to the list
stop_words2 <- stop_words %>% 
  bind_rows(my_stop_words)

#Unnested tokens data
bern_toke <- bern_trans %>% 
  unnest_tokens(word, speech, token = "words") %>% 
  anti_join(stop_words2) %>%  #Stop words
  arrange(word)

#manually removing numbers
bern_toke <- bern_toke %>% slice(380:nrow(bern_toke)) #Removes the numbers


### Julia Silge's Video mapped to my data


#this is exploring the td_idf
bern_toke %>% 
  count(debate_name,word, sort = TRUE) %>% 
  bind_tf_idf(word,debate_name,n) %>% #creating the tf_idf statistic on tokens
  group_by(debate_name) %>% 
  top_n(10) %>% #selecting the top 10 words
  ungroup %>% 
  mutate(word = reorder(word, tf_idf)) %>% #ordering it by their tf_idf rank
  ggplot(aes(word, tf_idf, fill = debate_name)) + #plotting it
  geom_col(show.legend = FALSE) +
  facet_wrap(~debate_name, scales = "free") + #by debate
  coord_flip()
  



#Topic Modeling
library(stm)
library(quanteda)

bern_dfm <- bern_toke %>% 
  count(debate_name,word,sort = TRUE) %>% 
  cast_dfm(debate_name,word,n)


topic_model <- stm(bern_dfm, K = 10, init.type = "Spectral") #Creating model with 5 topics
summary(topic_model)


td_beta <- tidy(topic_model)

td_beta %>% 
  group_by(topic) %>% 
  top_n(10) %>% 
  ungroup %>% 
  mutate(term = reorder(term, beta)) %>% #ordering it by their beta rank
  ggplot(aes(term, beta, fill = topic)) + #plotting it
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + #by topic
  coord_flip()

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(bern_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 5)



