library(ggplot2)
library(tidyr)
library(patchwork)

candidate_name <- c("Bernie Sanders")
cuttoffseconds <- 10

# Gets Candidate's Scripts ------------------------------------------------

source(here::here("R","candidate_scripts.R"))

candidate_transcripts <-
  get_candidate_transcripts(candidate_name, doc_col = TRUE)

candidate_token <-
  tokenize_transcripts(candidate_transcripts, rm_stop = TRUE, rm_num = TRUE)

# Descriptive Statistics --------------------------------------------------

# Creates a df with count of words per document as well as speaking time
# Nonunique words with dropped stopwords
words_per_document <-
  candidate_token %>%
  count(document, speaking_time_seconds)

#Creats a dot plot of words by speaking time
words_per_time_plot <-
  ggplot(words_per_document, aes(x = speaking_time_seconds, y = n)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Speaking Time in Seconds",
       y = "Words",
       title = "Words per Seconds of Speaking Time")

#Creates a distribution plot of the number of words per document
distribution_of_words <- ggplot(words_per_document, aes(x = n)) +
  geom_histogram(aes(y = ..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 1) +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(x = "Number of Words",
       y = "Density",
       title = "Number of words per Document")

# Creates a boxplot with the documents and words
boxplot_amt_words <-
  ggplot(words_per_document, aes(x = document, y = n)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  labs(x = "Documents",
       y = "# of Words",
       title = "Amount of words per document")

#Creates a plot with all three graphs
tri_plot <-
  boxplot_amt_words + distribution_of_words + words_per_time_plot +
  plot_annotation(title = "Descriptive Statistics of Documents",
                  subtitle = paste("Candidate:",candidate_name, "cutoff seconds >", cuttoffseconds))
tri_plot
summary(words_per_document)
nrow(words_per_document)

ggsave(here::here("output","graphs",paste("descriptive_",cuttoffseconds,"_cutoffseconds.png",sep = "")), tri_plot)
