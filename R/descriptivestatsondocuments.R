library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(patchwork)


#Gets Bernie's Scripts
source(here::here("R","berniescripts.R"))


# Descriptive Statistics --------------------------------------------------

# Creates a df with count of words per document as well as speaking time
# Nonunique words with dropped stopwords
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
tri_plot
