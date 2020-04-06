#See candidate for further list of libraries :)
library(stm)
library(furrr)
library(ggthemes)
library(tictoc)


#Sources scripts
source(here::here("R", "candidate_scripts.R"))

# Getting Everybody's Transcripts -----------------------------------------

#Get's cleaned transcripts
transcripts <- get_transcripts_clean()

#List of candidates that were still running by the last debate
list_of_candidates <- c("Bernie Sanders","Michael Bloomberg","Amy Klobuchar","Elizabeth Warren",
                        "Joe Biden","Pete Buttigieg","Tom Steyer")


#Get's the transcripts of all the candidates running by last debate
everybody_candidates <- 
  transcripts %>% 
  filter(speaker %in% list_of_candidates) %>% 
  filter(speaking_time_seconds >= 10)

#Creates a document column for the candidates
everybody_candidates <-
  everybody_candidates %>% 
  mutate(document = (1:nrow(everybody_candidates)))

#Creates a list of custom stop words
custom_stop_words <- tibble::tribble(
  ~word,        ~lexicon,
  "america",    "custom",
  "american",   "custom",
  "americans",  "custom",
  "people",     "custom",
  "country",    "custom",
  "bring",      "custom",
  "'",          "custom",
  "don",        "custom",
  "ve",         "custom",
  "crosstalk",  "custom",
  "ain",        "custom",
  "ll",         "custom",
  "didn",       "custom",
  "president",  "custom",
  "donald",     "custom",
  "time",       "custom",
  "tonight",    "custom",
)

#Adding my own stopwords to the list
stop_words2 <- 
  stop_words %>%
  bind_rows(custom_stop_words)

#Unnested tokens data
everbody_token <- everybody_candidates %>%
  unnest_tokens(word, speech, token = "words") %>%
  anti_join(stop_words2) %>%  #Stop words
  arrange(word)

#Gets rid of any digits for words
everbody_token <- 
  everbody_token %>%
  filter(!grepl("[[:digit:]]", word))


# Evaluating a K for topic models -----------------------------------------

#Creating dfm
everybody_dfm <- everbody_token %>% 
  count(document,word,sort = TRUE) %>% 
  tidytext::cast_dfm(document,word,n)

#Creates a cast_spares
everybody_sparse <- everbody_token %>%
  count(document, word) %>%
  tidytext::cast_sparse(document, word, n)

#Parellel computing
future::plan(multiprocess)

tic()
#Creating the models
many_models <-
  tibble(K = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)) %>%
  mutate(topic_model = future_map(K, ~ stm(
    everybody_sparse, K = .,
    verbose = FALSE,
    init.type = "Spectral")))
toc()

#Makes a heldout
heldout <- make.heldout(everybody_sparse)

#Creates a data table with different evaluation metrics
k_result <- many_models %>%
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, everybody_sparse),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, everybody_sparse),
    bound =  map_dbl(topic_model, function(x)
      max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x)
      lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x)
      length(x$convergence$bound))
  )

#Creates the data for a visualization
clustergraph_data <- k_result %>%
  transmute(
    K,
    `Lower bound` = lbound,
    Residuals = map_dbl(residual, "dispersion"),
    `Semantic coherence` = map_dbl(semantic_coherence, mean),
    `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
  ) %>%
  gather(Metric, Value,-K)

#Graphs the data for evaluation
clustergraph <- 
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
    title = "Model diagnostics by number of topics")

clustergraph

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



#Selects the desired number of clusters.
topic_model <- k_result %>%
 filter(K == 5) %>%
 pull(topic_model) %>%
 .[[1]]


# Graphing Topics ---------------------------------------------------------

#Making a tidy format for the beta measurment
td_beta <- tidy(topic_model)
td_gamma <- tidy(topic_model, matrix = "gamma",
                document_names = rownames(everybody_sparse))



top_terms <- td_beta %>%
 arrange(beta) %>%
 group_by(topic) %>%
 top_n(10, beta) %>%
 arrange(-beta) %>%
 select(topic, term) %>%
 summarise(terms = list(term)) %>%
 mutate(terms = map(terms, paste, collapse = ", ")) %>%
 unnest(cols = c(terms))

gamma_terms <- td_gamma %>%
 group_by(topic) %>%
 summarise(gamma = mean(gamma)) %>%
 arrange(desc(gamma)) %>%
 left_join(top_terms, by = "topic") %>%
 mutate(topic = paste0("Topic ", topic),
        topic = reorder(topic, gamma))

gamma_terms %>%
 top_n(10, gamma) %>%
 ggplot(aes(topic, gamma, label = terms, fill = topic)) +
 geom_col(show.legend = FALSE) +
 geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
           family = "IBMPlexSans") +
 coord_flip() +
 scale_y_continuous(expand = c(0,0),
                    labels = scales::percent_format()) +
 theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
 theme(plot.title = element_text(size = 16,
                                 family="IBMPlexSans-Bold"),
       plot.subtitle = element_text(size = 13)) +
 labs(x = NULL, y = expression(gamma),
      title = "Top 20 topics by prevalence in the debates")

