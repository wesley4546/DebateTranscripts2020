library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(lubridate)

#Gets Bernie's Scripts
source(here::here("R","berniescripts.R"))

#convert date(char) to date(date)
bern_token<-
  bern_token%>% 
  mutate(date = mdy(date))



# Word Cloud --------------------------------------------------------------

#Creating a word list with words being mentioned > 15 times
bern_word <- bern_token%>% 
  count(word) %>% 
  filter(n > 20) #set's it to only words that appear atleast 15


#Creating a word cloud
bern_cloud <- wordcloud(
  words = bern_word$word,
  freq = bern_word$n,
  color = brewer.pal(3,"Set2")
)



# Sentiment analysis ------------------------------------------------------



#Creating the sentiment data
bern_sent <- bern_token%>%
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
    title = "Bernie Sander's Overall Sentiment",
    y = "Overall Sentiment",
    x = "Debate Name"
  )



# Topic Modeling (LDA) ----------------------------------------------------
library(topicmodels)

#creating a df to work with
tidy_toke <-
  bern_token%>%
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
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta)) #term is subsituded for word due to the model

#Plotting
ggplot(word_prob, aes(term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()


