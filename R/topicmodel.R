library(stm)
library(furrr)

candidate_name <- c("Bernie Sanders")
number_of_clusters <- 7


file_name <- paste(tolower(str_replace_all(string=candidate_name, pattern=" ", repl="")))
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
topic_model <- stm(candidate_dfm,K = number_of_clusters, verbose = TRUE)

# Topic Plots -------------------------------------------------------------

#Making a tidy format for the beta measurment
td_beta <- tidy(topic_model)

# visualize the topics
topic_graph <- 
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
       subtitle = paste("Candidate:",candidate_name,"-","Number of topics:",number_of_clusters),
       x ='',
       y ='')

#Saves TD beta
ggsave(here::here("output","graphs","candidates",paste(file_name),"topics_tdbeta",paste(number_of_clusters,"cluster",paste(file_name),"topics.png", sep = "_")),topic_graph)

#Creates command dialoge box to notify once model is done.
system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE)
