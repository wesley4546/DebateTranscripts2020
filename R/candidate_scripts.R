library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)

# These are the scripts in which will be sourced within all files within project.
# 
# One issue I have is with the UTf-8 coding and a apostrophe. I just went
# around that problem the best I could. That means the script wont allow 
# Norah O'Donnell or Beto O'Rourke. If you have any suggestions in how I can fix this, 
# Please create an issue in GitHub or email me at: wesley.gardiner45456@gmail.com :)

cut_off_time <- 10

# get_Trancsripts function ------------------------------------------------

get_transcripts <- function(){
  
  #Gets transcripts
  transcripts <-
    read.csv(
      here::here("data", "raw", "debate_transcripts_v3_2020-02-26.csv"),
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
  
  return(transcripts)
  
}

# get_candidate_names function --------------------------------------------

get_speaker_names <- function(){
  
  #Load's Transcripts
  transcripts <- get_transcripts()
  
  #Retrieves all speaker names
  names <- unique(transcripts$speaker)

  return(names)
}

# get_candidate_transcripts function ------------------------------------------------

get_candidate_transcripts <- function(name, doc_col = FALSE){
  
  #get's transcripts
  transcripts <- get_transcripts()
  
  #Candidate's Name
  candidate_name <- name
  
  #Filters Transcripts to candidate's name
  candidate_transcripts <-
    transcripts %>%
    as_tibble() %>%
    filter(speaker == candidate_name)
  
  #document arugment if set to TRUE
  if (doc_col == TRUE) {
    #Creating a document column
    candidate_transcripts <-
      candidate_transcripts %>%
      mutate(document = (1:nrow(candidate_transcripts))) %>%
      as_tibble()
    
    #Grouping the transription by debate
    candidate_transcripts %>%
      group_by(document)
    
  }
  
  return(candidate_transcripts)
  
}

# tokenize_transcripts function ------------------------------------------------------------

tokenize_transcripts <- function(candidate_transcripts, rm_stop = FALSE, rm_num = FALSE){
  
  if(rm_stop == TRUE){
    #Creating my own stopwords
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
    stop_words2 <- stop_words %>%
      bind_rows(custom_stop_words)
    
    #Unnested tokens data
    candidate_token <- candidate_transcripts %>%
      unnest_tokens(word, speech, token = "words") %>%
      anti_join(stop_words2) %>%  #Stop words
      arrange(word)
    
  }
  else {
    #Unnested tokens data without stopwords
    candidate_token <- candidate_transcripts %>%
      unnest_tokens(word, speech, token = "words") %>%
      arrange(word)
  }
  
  
  if (rm_num == TRUE) {
    #Removes numbers from word column
    candidate_token <- candidate_token %>%
      filter(!grepl("[[:digit:]]", word))
  }
  
  #Changes date(char) to date(date)
  candidate_token<-
    candidate_token %>% 
    mutate(date = mdy(date))
  
  #Takes away documents with speaking time below the cut_off_time
  candidate_token <-
    candidate_token %>%
    filter(speaking_time_seconds > cut_off_time)
  
  
  
  return(candidate_token)
  
}



# test <-
#   tokenize_transcripts(
#     get_candidate_transcripts("Bernie Sanders", doc_col = TRUE),
#     rm_stop = TRUE,
#     rm_num = TRUE
#   )




# candidate_name <- c("Bernie Sanders")
# 
# # Gets Candidate's Scripts ------------------------------------------------
# 
# source(here::here("R","candidate_scripts.R"))
# 
# candidate_transcripts <-
#   get_candidate_transcripts(candidate_name, doc_col = TRUE)
# 
# candidate_token <-
#   tokenize_transcripts(candidate_transcripts, rm_stop = TRUE, rm_num = TRUE)


# Format candidate's name to a filename -----------------------------------

format_filename <- function(candidatesname){
  
  #Change's name to lowercase, and no space
  file_name <- paste(tolower(str_replace_all(string=candidatesname, pattern=" ", repl="")))
  
  return(file_name)
  
}
