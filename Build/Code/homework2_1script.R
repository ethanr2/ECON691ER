#Econ 691 Assignment #2
#Created by: Ethan Rahman
#Created on: 09/01

rm(list=ls())

library(rvest)
library(readr)
library(tidycensus)
library(sf)
library(tidyverse)

# Part 1 ------------------------------------------------------------------
#States assigned to me:
states <- c("maryland","pennsylvania","new-jersey","virginia","delaware")
#Fips code for each state:
state_nums <- c(24,42,34,51,10)

VOTES <- {}

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
  temp['state'] = state
  VOTES<-rbind(VOTES, temp)
}
#Cleaning up the environment by removing all irrelevant variables.
rm(temp, tables, webpage, results, url, state)
#Some county names are inconsistent with the census names, so we can fix them here. 
VOTES$County[which(VOTES$County=="DeWitt")]<-"De Witt"
VOTES$County[which(VOTES$County=="JoDaviess")]<-"Jo Daviess"
VOTES$County[which(VOTES$County=="LaClede")]<-"Laclede"
VOTES$County[which(VOTES$County=="LaRue")]<-"Larue"
VOTES$County[which(VOTES$County=="St. Louis City")]<-"St. Louis city"
VOTES$County[which(VOTES$County=="St. Louis County")]<-"St. Louis"
# Part 2 ------------------------------------------------------------------
#Function to scrape census data. The parameter t is the year of interest.
get_census_data <- function(t){
  #API Data
  vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
          "B02001_003","B05001_001","B05001_006","B07001_001", 
          "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")
  k<- 1
  for(num in state_nums){
    acs <- get_acs(geography = "county",
                   variables = vars,
                   state = num,
                   year = t,
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
      select("GEOID","NAME",starts_with("per"),"geometry")%>%
      mutate(state = states[k],
             county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
             county = trimws(gsub(" County", "", county))) %>%
      select(-c("NAME"))
      
    ifelse(k==1,
           census<-temp, 
           census<-rbind(census, temp))
    
    temp$area<-st_area(temp)
    map <- temp %>%
      summarise(area = sum(area)) %>%
      mutate(state = states[k])
    
    ifelse(k==1,
           MAP<-map, 
           MAP<-rbind(MAP, map))
    
    k<- k + 1
    rm(temp, map)
  }
  return(census)
}

CENSUS.1 <- get_census_data(2016)
CENSUS.2 <- get_census_data(2019)

rm(get_census_data)
save.image(file="./Build/Output/census.RData")

#Note: I split this into two separate files because there appears to be some 
#kind of glitch in one of the packages loaded in this script. Filtering will not
#work properly, please restart R and then run homework2_2script.R. 
