library(ggplot2)
library(wordcloud)
library(tidyr)
library(patchwork)

candidate_name <- c("Elizabeth Warren")

# Gets Candidate's Scripts ------------------------------------------------

source(here::here("R","candidate_scripts.R"))

candidate_transcripts <-
  get_candidate_transcripts(candidate_name, doc_col = TRUE)

candidate_token <-
  tokenize_transcripts(candidate_transcripts, rm_stop = TRUE, rm_num = TRUE)

# Word Cloud --------------------------------------------------------------


#Creating a word list with words being mentioned > 15 times
candidate_word <- candidate_token %>%
  count(word) %>%
  filter(n > 7) #set's it to only words that appear atleast 15

#Creating a word cloud
bern_cloud <- wordcloud(words = candidate_word$word,
                        freq = candidate_word$n,
                        color = brewer.pal(3, "Set2"),
                        random.order=FALSE,
                        rot.per=0,
                        scale=c(3,1),
                        max.words = 70)