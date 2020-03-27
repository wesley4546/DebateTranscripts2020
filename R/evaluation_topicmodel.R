library(stm)
library(furrr)
library(ggthemes)


#Name of candidate for model evaluation
candidate_name <- c("Michael Bloomberg")

#formats candidate's name for anyfile naming
file_name <- paste(tolower(str_replace_all(string=candidate_name, pattern=" ", repl="")))

#Which model is going to be evaluated (1 = macro view (2-100 clusters), 2 = zoomed (2-20 clusters))
picture <- 1


# Gets Candidate's Scripts ------------------------------------------------

#Sources scripts
source(here::here("R", "candidate_scripts.R"))

#Retrieve's candidate's transcripts
candidate_transcripts <-
  get_candidate_transcripts(candidate_name, doc_col = TRUE)

#Tokenizes transcripts
candidate_token <-
  tokenize_transcripts(candidate_transcripts, rm_stop = TRUE, rm_num = TRUE)

# Creating Multiple Models ------------------------------------------------

#Creating dfm
candidate_dfm <- candidate_token %>% 
  count(document,word,sort = TRUE) %>% 
  tidytext::cast_dfm(document,word,n)

#Creates a cast_spares
candidate_sparse <- candidate_token %>%
  count(document, word) %>%
  tidytext::cast_sparse(document, word, n)

#Parellel computing
future::plan(multiprocess)

#Creates models by cluster

if(picture == 1){
  
  many_models <-
    tibble(K = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20, 40, 50, 60, 70, 80, 100)) %>%
    mutate(topic_model = future_map(K, ~ stm(
      candidate_sparse, K = .,
      verbose = FALSE
    )))
  
} else{
  
  many_models <-
    data_frame(K = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) %>%
    mutate(topic_model = future_map(K, ~ stm(
      candidate_sparse, K = .,
      verbose = FALSE
    )))
  
  
}
  
#Makes a heldout
heldout <- stm::make.heldout(candidate_sparse)

#I have no idea what this is doing
k_result <- many_models %>%
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, candidate_sparse),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, candidate_sparse),
    bound =  map_dbl(topic_model, function(x)
      max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x)
      lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x)
      length(x$convergence$bound))
  )

# Evaluation Graphs -------------------------------------------------------

#I also have NO idea what this is doing...yet
clustergraph_data <- k_result %>%
  transmute(
    K,
    `Lower bound` = lbound,
    Residuals = map_dbl(residual, "dispersion"),
    `Semantic coherence` = map_dbl(semantic_coherence, mean),
    `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
  ) %>%
  gather(Metric, Value,-K)

#Creates a graph of the evaluation of the models
(clustergraph <- 
  clustergraph_data %>% 
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5,
            alpha = 0.7,
            show.legend = FALSE) +
  geom_point(alpha = 1, color = "Black") +
  facet_wrap( ~ Metric, scales = "free_y") +
  labs(
    x = "K (number of topics)",
    y = NULL,
    title = "Model diagnostics by number of topics",
    subtitle = paste("Candidate:",candidate_name)))



k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(5, 10, 20, 50, 100)) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

clustergraph

# Saving Graph ------------------------------------------------------------

ggsave(
  here::here(
    "output",
    "graphs",
    "candidates",
    paste(file_name),
    paste(picture,"evaluation",paste(file_name),"kclustersCUTOFF.png", sep = "_")
  ),
  clustergraph
)

#Creates command dialoge box to notify once model is done.
system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE)

# Selecting Topic Number --------------------------------------------------


# 
# topic_model <- k_result %>% 
#   filter(K == 60) %>% 
#   pull(topic_model) %>% 
#   .[[1]]
# 
# 
# 
# # Graphing Topics ---------------------------------------------------------
# 
# #Making a tidy format for the beta measurment
# td_beta <- tidy(topic_model)
# td_gamma <- tidy(topic_model, matrix = "gamma",
#                  document_names = rownames(candidate_sparse))
# 
# 
# 
# top_terms <- td_beta %>%
#   arrange(beta) %>%
#   group_by(topic) %>%
#   top_n(7, beta) %>%
#   arrange(-beta) %>%
#   select(topic, term) %>%
#   summarise(terms = list(term)) %>%
#   mutate(terms = map(terms, paste, collapse = ", ")) %>% 
#   unnest(cols = c(terms))
# 
# gamma_terms <- td_gamma %>%
#   group_by(topic) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   left_join(top_terms, by = "topic") %>%
#   mutate(topic = paste0("Topic ", topic),
#          topic = reorder(topic, gamma))
# 
# gamma_terms %>%
#   top_n(20, gamma) %>%
#   ggplot(aes(topic, gamma, label = terms, fill = topic)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
#             family = "IBMPlexSans") +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0, 0.09),
#                      labels = scales::percent_format()) +
#   theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
#   theme(plot.title = element_text(size = 16,
#                                   family="IBMPlexSans-Bold"),
#         plot.subtitle = element_text(size = 13)) +
#   labs(x = NULL, y = expression(gamma),
#        title = "Top 20 topics by prevalence in the debates",
#        subtitle = paste("Candidate:",candidate_name))
