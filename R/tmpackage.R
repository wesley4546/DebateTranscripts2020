library(tidyverse)
library(tm)
library(ggplot2)
library(wordcloud)


#Initial Data
transripts <- read.csv("Democratic Debate 2020 transcripts/debate_transcripts_v2_2020-02-23.csv", stringsAsFactors = FALSE)


#Bernie's Transcripts
bern_trans <- 
  transripts %>% 
  as_tibble() %>% 
  filter(speaker == c("Bernie Sanders"))

#Making Vector/Corups
bern_vect <- VectorSource(bern_trans)
bern_corp <- VCorpus(bern_vect)

#Creating function to clean
clean_corpus <- function(corpus){
  
  corpus <- corpus %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, words = c(stopwords("en"))) %>% 
    tm_map(stripWhitespace)
  
  return(corpus)
  
}

#Cleaned
bern_clean <- clean_corpus(bern_corp)

bern_tdm <- TermDocumentMatrix(bern_clean)
bern_m <- as.matrix(bern_tdm)

bern_terms <- Terms(bern_tdm)

bern_tidy <- tidy(bern_tdm)
