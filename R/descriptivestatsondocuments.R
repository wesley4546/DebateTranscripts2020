library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(patchwork)

# Getting Transcripts -----------------------------------------------------

#Gets transcripts
transripts <-
  read.csv(
    here::here("data", "raw", "debate_transcripts_v3_2020-02-26.csv"),
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )



# Getting Bernie's Transcripts --------------------------------------------

#Bernie's Transcripts
bern_transcript <- 
  transripts %>% 
  as_tibble() %>% 
  filter(speaker == c("Bernie Sanders"))


#Creating a document column
bern_transcript <-
  bern_transcript %>%
  mutate(document = (1:nrow(bern_transcript))) %>%
  as_tibble()


#Grouping the transription by debate
bern_transcript %>% 
  group_by(document)



# Cleaning Bernie's Scripts ---------------------------------------------------

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
  "crosstalk",  "custom"
)


#Adding my own stopwords to the list
stop_words2 <- stop_words %>% 
  bind_rows(my_stop_words)


#Unnested tokens data
bern_token <- bern_transcript %>% 
  unnest_tokens(word, speech, token = "words") %>% 
  anti_join(stop_words2) %>%  #Stop words
  arrange(word)


#manually removing numbers
bern_token <- bern_token %>% slice(380:nrow(bern_token)) #Removes the numbers (1:380)




# Descriptive Statistics --------------------------------------------------

#Creates a df with count of words per document as well as speaking time
words_per_document <-
  bern_token %>%
  count(document, speaking_time_seconds)
summary(words_per_document)


#createas lm with n by speaking time
wcount_by_speakingtime <- lm(n ~ speaking_time_seconds, words_per_document)


#Creats a dot plot of words by speaking time
words_per_time_plot <-
  ggplot(words_per_document, aes(x = speaking_time_seconds, y = n)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Speaking Time in Seconds",
       y = "Words",
       title = "Bernie's Words per Seconds of Speaking Time")
words_per_time_plot


#Creates a distribution plot of the number of words per document
distribution_of_words <- ggplot(words_per_document, aes(x = n)) +
  geom_histogram(aes(y = ..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 1) +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(x = "Number of Words",
       y = "Density",
       title = "Distribution of Number of words per Document")
distribution_of_words


# Creates a boxplot with the documents and words
boxplot_amt_words <- ggplot(words_per_document, aes(x = document, y = n)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  labs(x = "Documents",
       y = "# of Words",
       title = "Boxplot of Amount of words per document")
boxplot_amt_words


#Creates a plot with all three graphs
tri_plot <- boxplot_amt_words + distribution_of_words + words_per_time_plot

























