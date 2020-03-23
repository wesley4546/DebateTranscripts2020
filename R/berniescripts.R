library(tidyverse)
library(tidytext)

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
  "crosstalk",  "custom",
  "ain",        "custom",
  "ll",         "custom",
  "didn",       "custom"
)


#Adding my own stopwords to the list
stop_words2 <- stop_words %>% 
  bind_rows(my_stop_words)


# Tokenization ------------------------------------------------------------


#Unnested tokens data
bern_token <- bern_transcript %>% 
  unnest_tokens(word, speech, token = "words") %>% 
  anti_join(stop_words2) %>%  #Stop words
  arrange(word)


#manually removing numbers
bern_token <- bern_token %>% slice(380:nrow(bern_token)) #Removes the numbers (1:380)

