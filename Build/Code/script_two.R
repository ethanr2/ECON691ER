#This is a script for introducing scripts
#Created by: Ethan Rahman
#Created on: 09/01

rm(list=ls())

library(tidyverse)
library(rvest)
library(readr)

states <- c("illinois", "indiana", "kentucky", "missouri", "wisconsin", "iowa")
for(state in states){

  url <- paste0("https://www.nytimes.com/elections/2016/results/", state) 
  webpage <- read_html(url)
  tables <- webpage %>%
    html_nodes("table")
  
  (results <- tables[2] %>%
    html_table(header = TRUE, fill = TRUE) %>%
    as.data.frame())
  
  (temp<- results %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","", Clinton)),
           "Trump" = as.numeric(gsub(",","", Trump)),
           "pctClinton" = Clinton/(Clinton + Trump),
           "pctTrump" = Trump/(Clinton + Trump)))
  assign(state, temp)
}