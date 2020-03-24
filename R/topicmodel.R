library(ggplot2)
library(tidyr)
library(stm)
library(furrr)

candidate_name <- c("Andrew Yang")

# Gets Candidate's Scripts ------------------------------------------------

#Sources scripts
source(here::here("R", "candidate_scripts.R"))

#Retrieve's candidate's transcripts
candidate_transcripts <-
  get_candidate_transcripts(candidate_name, doc_col = TRUE)

#Tokenizes transcripts
candidate_token <-
  tokenize_transcripts(candidate_transcripts, rm_stop = TRUE, rm_num = TRUE)

# Creating Model ----------------------------------------------------------

#Creating a dfm
candidate_dfm <- candidate_token %>% 
  count(document,word,sort = TRUE) %>% 
  tidytext::cast_dfm(document,word,n)

#Creates a cast_spares
candidate_sparse <- candidate_token %>%
  count(document, word) %>%
  tidytext::cast_sparse(document, word, n)

#running the topic model
topic_model <- stm(candidate_dfm,K = 6, init.type = "Spectral", verbose = TRUE)

# Topic Plots -------------------------------------------------------------

#Making a tidy format for the beta measurment
td_beta <- tidy(topic_model)

# visualize the topics
td_beta %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder(term, beta)) %>% #ordering it by their beta rank
  ggplot(aes(term, beta, fill = topic)) + #plotting it
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free") + #by topic
  coord_flip() +
  labs(title = "Spectral Topic Model",
       subtitle = paste("Candidate:",candidate_name),
       x ='',
       y ='')

#Measurement of the model using histograms
td_gamma <- tidy(topic_model,
                 matrix = "gamma",
                 document_names = rownames(candidate_dfm))

#Creates histogram plot
ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 5)