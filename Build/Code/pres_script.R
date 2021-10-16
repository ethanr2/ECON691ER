rm(list=ls())

library(tidyverse)
library(scales)
library(readr)
library(sf)
library(cowplot)
library(tidycensus)
query = FALSE
if (query){
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
  cap_df <- cap_df %>%
    mutate(city_name = gsub(" city","", city_name))
  cap_df <- cbind(cap_df, st_coordinates(st_centroid(cap_df)))
  rm(k, temp)
  save.image(file="./Build/Output/presentation_data.RData")
} else{
  load(file="./Build/Output/presentation_data.RData")
}

cap_df["x_offset"] <- cap_df["X"]
cap_df["y_offset"] <- cap_df["Y"]
#
adj_pos <- function(name,x_inc, y_inc, cap_df){
  #ax <- paste(ax,  "_offset", sep ="")
  cap_df$x_offset[which(cap_df$city_name==name)] <- x_inc + cap_df$x_offset[which(cap_df$city_name==name)]
  cap_df$y_offset[which(cap_df$city_name==name)] <- y_inc + cap_df$y_offset[which(cap_df$city_name==name)]
  return(cap_df)
}

cap_df <- adj_pos("Richmond", 
                  -.65, 0, cap_df)
cap_df <- adj_pos("Annapolis", 
                  -.55, .10, cap_df)
cap_df <- adj_pos("Harrisburg", 
                  -.65, 0, cap_df)
cap_df <- adj_pos("Trenton", 
                  0, .25, cap_df)
cap_df <- adj_pos("Dover", 
                  .25, -.25, cap_df)
# state_caps <- c("Annapolis city", 
#                 "Harrisburg city",
#                 "Trenton city",
#                 "Richmond city",
#                 "Dover city")
(cap_df[,c("city_name", "X","x_offset", "y_offset")])
map1<-ggplot(state_df)+
  ggtitle("Mid-Atlantic States and Their Capitals") +
  geom_sf(aes(geometry = geometry, fill = NAME))+
  geom_sf(data = cap_df, 
          aes(geometry = geometry),
          fill = "black", 
          color = "black")+
  geom_text(data = cap_df, aes(x_offset, y_offset, label = city_name), fontface = "bold",size = 3) +
  theme(legend.position = "none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
show(map1)
