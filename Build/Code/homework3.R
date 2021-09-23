#Econ 691 Assignment #3
#Created by: Ethan Rahman
#Created on: 09/22

rm(list=ls())

library(readr)
library(tidyverse)

load(file="./Build/Output/census.RData")
rm(CENSUS.1, VOTES, query, get_census_data, get_vote_data)
# Part 1 ------------------------------------------------------------------
raw <- read_csv("Data/countypres_2000-2020.csv")
df <- read_csv("Data/countypres_2000-2020.csv") %>%
  mutate(state= tolower(state),
         state = gsub("new jersey", "new-jersey",state),
         county = tolower(county_name)) %>%
  subset(state %in% states &
           (year==2016 | year == 2020) &
           (office == "PRESIDENT" | office =="US PRESIDENT") &
           (party =="DEMOCRAT" | party == "REPUBLICAN")) %>%
  select(c("year",
           "state",
           "county", 
           "candidate", 
           "party",
           "candidatevotes", "mode"))
#Cleaning up some naming inconsistencies
#In 2020, many virginia counties appended "city" to their names
bools <- grepl("^.+(city)$",df$county) & 
  df$state == "virginia" &
  df$year == 2020
df[bools,] <- df[bools,] %>% 
  mutate(county = gsub(" city", "", county))

df <- df %>%
  group_by(year,state,county) %>%
  mutate(sumvotes = sum(candidatevotes),
         candidatevotes = candidatevotes/sumvotes)%>%
  group_by(year,state,county,party) %>%
  summarise(candidatevotes = sum(candidatevotes))
print(grepl("^.+(city)$", df$county))

(VOTES.2020 <- df %>%
  subset(year==2020) %>%
  group_by(state,county,party))
(VOTES.2016 <- df %>%
  subset(year ==2016)%>%
  group_by(state,county,party))

#Setting up the dataframe with VOTES.2020 as a template
(D_VOTES <- merge(VOTES.2020,
                 VOTES.2016,
                 by.x=c("state", "county","party"), 
                 by.y=c("state", "county","party")) %>%
  mutate(vote_change = candidatevotes.x - candidatevotes.y))





