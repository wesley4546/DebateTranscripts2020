library(tidyverse)
library(tidytext)

# These are the scripts in which will be sourced within all files within project.
# 
# One issue I have is with the UTf-8 coding and a apostrophe. I just went
# around that problem the best I could. That means the script wont allow 
# Norah O'Donnell or Beto O'Rourke. If you have any suggestions in how I can fix this, 
# Please create an issue in GitHub or email me at: wesley.gardiner45456@gmail.com :)


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
      "people",     "custom",
      "country",    "custom",
      "bring",      "custom",
      "'",          "custom",
      "don",        "custom",
      "ve",         "custom",
      "crosstalk",  "custom",
      "ain",        "custom",
      "ll",         "custom",
      "didn",       "custom"
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
  
  return(candidate_token)
  
}

