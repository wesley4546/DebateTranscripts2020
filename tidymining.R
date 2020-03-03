library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(lubridate)

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

#convert date(char) to date(date)
bern_toke <-
  bern_toke %>% 
  mutate(date = mdy(date))

#Creating a word list with words being mentioned > 15 times
bern_word <- bern_toke %>% 
  count(word) %>% 
  filter(n > 20) #set's it to only words that appear atleast 15

#Creating a word cloud
bern_cloud <- wordcloud(
  words = bern_word$word,
  freq = bern_word$n,
  color = brewer.pal(3,"Set2")
)



#Creating the sentiment data
bern_sent <- bern_toke %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment,debate_name, date) %>%
  spread(sentiment, n) %>%
  mutate(overall_seniment = positive - negative)%>% 
  mutate(debate_name2 = fct_reorder(debate_name, date))  #had to reorder by date

#Creating Sentiment analysis graph
ggplot(bern_sent, aes(x=debate_name2, y = overall_seniment, fill = as.factor(debate_name2))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Bernie Sander's Overall Sentiment during recent debates",
    y = "Overall Sentiment",
    x = "Debate Name"
  )


## Topic Modeling
library(topicmodels)

#creating a df to work with
tidy_toke <-
  bern_toke  %>%
  count(word, debate_name) %>%
  select(debate_name, everything()) #rearranging for clarity

#Creating a DTM
dtm_bern <-
  tidy_toke %>%
  cast_dtm(debate_name, word, n)
set.seed(1) #We have to set a seed for control

# RunningLDA
lda_bern <- LDA(dtm_bern, k = 8, method = "Gibbs")

#Tidying up LDA for visualization
tidy_lda <-
  lda_bern %>%
  tidy(matrix = "beta") %>%
  arrange(desc(beta))

#Creating a df for visualization (with factor leveling)
word_prob <-
  tidy_lda %>%
  group_by(topic) %>%  #Automatically named topic due to model
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta)) #term is subsituded for word due to the model

#Plotting
ggplot(word_prob, aes(term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()


