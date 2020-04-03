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



# get_trancsripts function ------------------------------------------------

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



# get_transcripts_clean ---------------------------------------------------

get_transcripts_clean <- function(){
  
  #reads in transcripts
  transcripts <- get_transcripts()
  
  #transforms date to date
  transcripts<-
    transcripts %>% 
    mutate(date = mdy(date))
  
}

# get_candidate_names function --------------------------------------------

get_speaker_names <- function(){
  
  #Load's Transcripts
  transcripts <- get_transcripts()
  
  #Retrieves all speaker names
  names <- unique(transcripts$speaker)

  return(names)
}


# Format candidate's name to a filename -----------------------------------

format_filename <- function(candidatesname){
  
  #Change's name to lowercase, and no space
  file_name <- paste(tolower(str_replace_all(string=candidatesname, pattern=" ", repl="")))
  
  return(file_name)
  
}
