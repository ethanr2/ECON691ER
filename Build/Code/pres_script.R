rm(list=ls())

library(tidyverse)
library(scales)
library(readr)
library(sf)
library(cowplot)

rm(list=ls())

library(tidyverse)
library(scales)
library(readr)
library(sf)
library(cowplot)

load(file="./Build/Output/census_processed.RData") # From homework2.R
rm(CENSUS.1, core, map1, map2, VOTES)
#States assigned to me:
states <- c("maryland","pennsylvania","new-jersey","virginia","delaware")
#Fips code for each state:
state_nums <- c(24,42,34,51,10)
state_caps <- c("Annapolis city", 
                "Harrisburg city",
                "Trenton city",
                "Richmond city",
                "Dover city")
library(tidycensus)
# Intro Pictures

state_df <- get_acs(geography = "state",
             variables = "B01001_001",
             state = state_nums,
             year = 2019,
             geometry = TRUE)
place_df <-get_acs(geography = "place",
                   variables = "B01001_001",
                   state = state_nums,
                   year = 2019,
                   geometry = TRUE) %>%
  mutate(city_name = as.data.frame(str_split_fixed(NAME, ",", 2))[,1])

k <- 1
for(cap in state_caps){
  temp <- place_df %>%
    subset(place_df$city_name == cap)
  if (k ==1){
    cap_df <- temp
  } else{
    cap_df <- rbind(cap_df, temp)
  }
  k = k+1
}

map1<-ggplot(CENSUS.2)+
  ggtitle("Mid-Atlantic States") +
  geom_sf(aes(geometry = geometry, fill = state))+
  geom_sf(data = cap_df, aes(geometry = cap_df$geometry, color = "black"))
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
show(map1)
