#Econ 691 Assignment #3
#Created by: Ethan Rahman
#Created on: 09/22

rm(list=ls())

library(tidyverse)
library(scales)
library(readr)
library(sf)
library(cowplot)

load(file="./Build/Output/census_processed.RData") # From homework2.R
rm(CENSUS.1, core, map1, map2, VOTES, state_nums)
# Part 1 ------------------------------------------------------------------
df <- read_csv("Data/countypres_2000-2020.csv") %>%
  mutate(state= tolower(state),
         state = gsub("new jersey", "new-jersey",state)) %>%
  subset(state %in% states &
           (year==2016 | year == 2020) &
           (office == "PRESIDENT" | office =="US PRESIDENT") &
           (party =="DEMOCRAT" | party == "REPUBLICAN")) %>%
  select(c("year",
           "state",
           "county_fips",
           "party",
           "mode",
           "candidatevotes")) %>%
  group_by(year,state,county_fips, party) %>%
  summarise(candidatevotes = sum(candidatevotes)) %>%
  filter(!is.na(candidatevotes))


(VOTES.2020 <- df %>%
  subset(year==2020))
(VOTES.2016 <- df %>%
  subset(year ==2016))

(D_VOTES <- merge(VOTES.2020,
                 VOTES.2016,
                 all = TRUE,
                 by.x=c("state", "county_fips", "party"), 
                 by.y=c("state", "county_fips","party")) %>%
  mutate(vote_change = candidatevotes.x- candidatevotes.y) %>%
  select(c("state", "county_fips","party","vote_change")) %>%
  reshape(timevar = "party",
          idvar = c("state", "county_fips"),
          direction = "wide")) 
    

# Part 2 ------------------------------------------------------------------

core <- merge(D_VOTES,
                 CENSUS.2,
                 by.x = c("state", "county_fips"),
                 by.y = c("state", "GEOID")) 
#%>%
#  mutate(geometry = st_as_sf(geometry))
map1<-ggplot(core)+
  ggtitle("Democratic Presidential Candidate Votes: 2016 to 2020") +
  geom_sf(aes(geometry = geometry, fill = vote_change.DEMOCRAT))+
  scale_fill_gradient(low="white",
                      high="blue",
                      label = comma,
                      aes(name="Change in Votes"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
map2<-ggplot(core)+ 
  ggtitle("Republican Presidential Candidate Votes: 2016 to 2020")+
  geom_sf(aes(geometry = geometry, fill = vote_change.REPUBLICAN))+
  scale_fill_gradient(low="white",
                      high="red",
                      label = comma,
                      aes(name="Change in Votes"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(map1,map2)

