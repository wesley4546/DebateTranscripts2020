library(tidyverse)
library(tidytext)
library(ggplot2)
library(tidyr)



#Gets Bernie's Scripts
source(here::here("R","berniescripts.R"))


# Julia Silge's Video -----------------------------------------------------

#this is exploring the tf_idf
bern_token %>% 
  count(document,word, sort = TRUE) %>% 
  bind_tf_idf(word,document,n) %>% #creating the tf_idf statistic on tokens
  group_by(document) %>% 
  top_n(10) %>% #selecting the top 10 words
  ungroup %>% 
  mutate(word = reorder(word, tf_idf)) #ordering it by their tf_idf rank

# I can't plot 430 documents
# ggplot(aes(word, tf_idf, fill = document)) + #plotting it
# geom_col(show.legend = FALSE) +
# facet_wrap(~document, scales = "free") + #by debate
# coord_flip()

bern_tf_idf <- 
  bern_token %>% 
  count(document,word, sort = TRUE) %>% 
  bind_tf_idf(word,document,n)


#Topic Modeling
library(stm)


#Creating a dfm
bern_dfm <- bern_token %>% 
  count(document,word,sort = TRUE) %>% 
  cast_dfm(document,word,n)



# Julia Silge's Blog Post -------------------------------------------------

#Creates a cast_spares
bernie_trans_sparse <- bern_token %>%
  count(document, word) %>%
  cast_sparse(document, word, n)


#Parellel computing
library(furrr)
plan(multiprocess)


#Make a tibble with models with k = 0 through 20 to test for amount of clustering
many_models <-
  tibble(K = c(seq(0, 70, by = 10))) %>%
  mutate(topic_model = future_map(
    K, ~ stm(bernie_trans_sparse,
             K = .,
             init.type = "Spectral",
             verbose = TRUE)
    )
  )


#Makes a heldout
heldout <- make.heldout(bernie_trans_sparse)


#I have no idea what this is doing
k_result <- many_models %>%
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, bernie_trans_sparse),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, bernie_trans_sparse),
    bound =  map_dbl(topic_model, function(x)
      max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x)
      lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x)
      length(x$convergence$bound))
  )

k_result


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


clustergraph <- 
  clustergraph_data %>% 
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5,
            alpha = 0.7,
            show.legend = FALSE) +
  facet_wrap( ~ Metric, scales = "free_y") +
  labs(
    x = "K (number of topics)",
    y = NULL,
    title = "Model diagnostics by number of topics",
    subtitle = "These diagnostics indicate a good number of topics."
  )
clustergraph



# Julia's Video (Continued) -----------------------------------------------

#running the topic model
topic_model <- stm(bern_dfm,K = 4, init.type = "Spectral", verbose = TRUE) #Creating model with 5 topics


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
  labs(title = "k = 10 topics for Bernie's Transcript data")

#Measurement of the model using histograms
td_gamma <- tidy(topic_model,
                 matrix = "gamma",
                 document_names = rownames(bern_dfm))

#Creates histogram plot
ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 5)



