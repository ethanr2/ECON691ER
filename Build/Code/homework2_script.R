#This is a script for introducing scripts
#Created by: Ethan Rahman
#Created on: 09/01

rm(list=ls())

library(tidyverse)
library(rvest)
library(readr)
library(tidycensus)
states <- c("maryland","pennsylvania","new-jersey","virginia","delaware")
state_nums <- c(24,42,34,51,10)
df <- list()
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
  temp['state'] = state
  df<-rbind(df, temp)
}

#API Data
vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")
census.1 <- list()
for(num in state_nums){
  acs <- get_acs(geography = "county",
                 variables = vars,
                 state = num,
                 year = 2016,
                 geometry = TRUE)
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry")
  
  census.1 <- rbind(census.1, temp)
}
census.1


