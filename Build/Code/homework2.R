#Econ 691 Assignment #2
#Created by: Ethan Rahman
#Created on: 09/01

rm(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rvest)
library(readr)
library(tidycensus)
library(sf)
library(cowplot)

#Set this to true if you want to requery the census data. 
query = TRUE

# Part 1 ------------------------------------------------------------------
#States assigned to me:
states <- c("maryland","pennsylvania","new-jersey","virginia","delaware")
#Fips code for each state:
state_nums <- c(24,42,34,51,10)

get_vote_data <- function(){
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
  return(VOTES)
}


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
  
    k<- k + 1
  }
  return(census)
}
if (query){
  VOTES <- get_vote_data()
  CENSUS.1 <- get_census_data(2016)
  CENSUS.2 <- get_census_data(2019)
  save.image(file="./Build/Output/census.RData")
} else{
  load(file="./Build/Output/census.RData") 
}
#Cleaning up the environment by removing all irrelevant variables.
rm(get_census_data, get_vote_data, query)

#Clean up the dataframes to prepare for merge.
CENSUS.1 <- CENSUS.1 %>%
  mutate(county = gsub("city", "City", county))
CENSUS.2 <- CENSUS.2 %>%
  mutate(county = gsub("city", "City", county))

VOTES$County[which(VOTES$County=="Alexandria")]<-"Alexandria City"
VOTES$County[which(VOTES$County=="Bristol")]<-"Bristol City"
VOTES$County[which(VOTES$County=="Buena Vista")]<-"Buena Vista City"
VOTES$County[which(VOTES$County=="Charlottesville")]<-"Charlottesville City"
VOTES$County[which(VOTES$County=="Chesapeake")]<-"Chesapeake City"
VOTES$County[which(VOTES$County=="Colonial Heights")]<-"Colonial Heights City"
VOTES$County[which(VOTES$County=="Covington")] <- "Covington City"
VOTES$County[which(VOTES$County=="Danville")] <- "Danville City"
VOTES$County[which(VOTES$County=="Emporia")] <- "Emporia City"
VOTES$County[which(VOTES$County=="Falls Church")] <- "Falls Church City"
VOTES$County[which(VOTES$County=="Fredericksburg")] <- "Fredericksburg City"
VOTES$County[which(VOTES$County=="Galax")] <- "Galax City"
VOTES$County[which(VOTES$County=="Hampton")] <- "Hampton City"
VOTES$County[which(VOTES$County=="Harrisonburg")] <-"Harrisonburg City"
VOTES$County[which(VOTES$County=="Hopewell")] <-"Hopewell City"
VOTES$County[which(VOTES$County=="Lexington")] <-"Lexington City"
VOTES$County[which(VOTES$County=="Manassas")] <-"Manassas City"
VOTES$County[which(VOTES$County=="Manassas Park")] <-"Manassas Park City"
VOTES$County[which(VOTES$County=="Lynchburg")] <-"Lynchburg City"
VOTES$County[which(VOTES$County=="Martinsville")] <-"Martinsville City"
VOTES$County[which(VOTES$County=="Newport News")] <-"Newport News City"
VOTES$County[which(VOTES$County=="Norfolk")] <-"Norfolk City"
VOTES$County[which(VOTES$County=="Norton")] <-"Norton City"
VOTES$County[which(VOTES$County=="Petersburg")] <-"Petersburg City"
VOTES$County[which(VOTES$County=="Poquoson")] <-"Poquoson City"
VOTES$County[which(VOTES$County=="Portsmouth")] <-"Portsmouth City"
VOTES$County[which(VOTES$County=="Radford")] <-"Radford City"
VOTES$County[which(VOTES$County=="Staunton")] <-"Staunton City"
VOTES$County[which(VOTES$County=="Suffolk")] <-"Suffolk City"
VOTES$County[which(VOTES$County=="Salem" & VOTES$state =="virginia")] <-"Salem City"
VOTES$County[which(VOTES$County=="Virginia Beach")] <-"Virginia Beach City"
VOTES$County[which(VOTES$County=="Waynesboro")] <-"Waynesboro City"
VOTES$County[which(VOTES$County=="Williamsburg")] <-"Williamsburg City"
VOTES$County[which(VOTES$County=="Winchester")] <-"Winchester City"
#Calculate the difference in percentage for each statistic.
non_nums <- c("GEOID", "state", "county", "geometry") #Non-numeric column labels
CENSUS.3 <- CENSUS.1[non_nums]
#Numeric column labels
cols <- CENSUS.1 %>% 
  st_drop_geometry() %>%
  select(!c("GEOID", "state", "county")) %>% 
  colnames()
CENSUS.3[cols] = st_drop_geometry(CENSUS.2[cols]) - st_drop_geometry(CENSUS.1[cols])
#Merge CENSUS.3 with VOTES. 
core <- merge(CENSUS.3, 
              VOTES, 
              by.x=c("state", "county"), 
              by.y=c("state", "County"),
              all =TRUE)

rm(cols, non_nums)

# Part 3 ------------------------------------------------------------------
map1<-ggplot(core)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",
                      high="blue",
                      limits=c(0,1),
                      labels = scales::percent,
                      aes(name="Clinton Vote Share"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
map2<-ggplot(core)+ 
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",
                      high="white",
                      labels = scales::percent,
                      aes(name="Change in White Population"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(map1,map2)


